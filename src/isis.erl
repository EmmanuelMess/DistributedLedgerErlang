-module(isis).

-define(DEBUG, false).

-record(queuedMessage, { message, id, senderPId, sequenceNumber, suggesterPId, status }).

-export([start/0, finish/0, broadcast/1, reciever/0, counter/1, queue/1, p/1]).

% Inicia el isis
start() ->
  if
    ?DEBUG -> net_kernel:start([shortName(), shortnames]);
    true -> ok
  end,
  register(sequenceNumberer, spawn(?MODULE, counter, [0])),
  register(messageIdGenerator, spawn(?MODULE, counter, [0])),
  register(holdBackQueue, spawn(?MODULE, queue, [[]])),
  register(messagesReciever, spawn(?MODULE, reciever, [])).

% Termina el isis
finish() ->
  sequenceNumberer ! die,
  messageIdGenerator ! die,
  holdBackQueue ! die,
  messagesReciever ! die,
  unregister(sequenceNumberer),
  unregister(messageIdGenerator),
  unregister(holdBackQueue),
  unregister(messagesReciever).

% Hace un broadcast a todos los nodos conectados,
% estos deben tener el modulo iniciado y funcional
broadcast(Record) ->
  messageIdGenerator ! { getAndInc, self() },
  MessageId = receive { counter_getAndInc_response, C } -> C end,
  lists:foreach(fun(Node) -> { messagesReciever, Node } ! { message, Record, MessageId, self() } end, [node()] ++ nodes()),

  recieveSuggestions(MessageId).

% Recibe sugerencias de numeros
recieveSuggestions(MessageId) ->
  Suggestions = lists:map(
    fun ReceiveSuggetion(Node) ->
      receive
        { message_response, MessageId, SuggestedSequenceNumber, PId } ->
          { SuggestedSequenceNumber, PId }
      after 1000 ->
        IsAlive = length(lists:filter(fun(N) -> N =:= Node end, allNodes())) =/= 0,
        if
          IsAlive -> ReceiveSuggetion(Node);
          true -> -1
        end
      end
    end,
    allNodes()),

  HighestSequenceNumber = lists:foldr(
    fun (X, Y) ->
      {SuggestedX, _} = X,
      {SuggestedY, _} = Y,
      if
        SuggestedX < SuggestedY -> Y;
        true                    -> X
      end
    end,
    { -1, 0},
    Suggestions),

  { SequenceNumber, ProposingPId } = HighestSequenceNumber,
  SequencedMessage = { message_sequenced, MessageId, SequenceNumber, self(), ProposingPId },
  lists:foreach(fun(Node) -> { messagesReciever, Node } !  SequencedMessage end, allNodes()).

% Esta es la parte interna, recibe peticiones de numero de secuencia y
% responde
reciever() ->
  receive
    { message, Message, MessageId, SenderPId } ->
      sequenceNumberer ! { getAndInc, self() },
      SequenceNumber = receive { counter_getAndInc_response, C } -> C end,
      SenderPId ! { message_response, MessageId, SequenceNumber, self() },
      M = #queuedMessage{ message = Message, id = MessageId, senderPId = SenderPId, sequenceNumber =  SequenceNumber, suggesterPId = self(), status = undeliverable },
      holdBackQueue ! { add, M };

    { message_sequenced, MessageId, SequenceNumber, SenderPId, ProposingPId } ->
      sequenceNumberer ! { conditionalSet, SequenceNumber },
      holdBackQueue ! { remove, MessageId, SenderPId, self() },
      MessageObject = receive { remove_response, M } -> M end,
      NewMessageObject = MessageObject#queuedMessage{ sequenceNumber = SequenceNumber, suggesterPId = ProposingPId, status = deliverable},
      holdBackQueue ! { add, NewMessageObject };

    die -> ok
  end,
  reciever().

% Mantiene la cola de mensajes entrantes,
% se encarga de mandar los mensajes marcados para deliver
queue(Queue) ->
  % Agrega al final, elimina del principio
  receive
    { add, Element } ->
      Ordering =
        fun(A, B) ->
          if
            A#queuedMessage.sequenceNumber =/=  B#queuedMessage.sequenceNumber ->
              A#queuedMessage.sequenceNumber < B#queuedMessage.sequenceNumber;
            true ->
              if
                A#queuedMessage.status =/= B#queuedMessage.status -> A#queuedMessage.status =:= undeliverable;
                true -> node(A#queuedMessage.senderPId) < node(B#queuedMessage.senderPId)
              end
          end
        end,
      Sorted = lists:sort(Ordering, Queue ++ [Element]),
      SortedUndeliverable = deliverDeliverable(Sorted),
      queue(SortedUndeliverable);

    { remove, MessageId, SenderPId, PId } ->
      IsItTheMessage = fun(X) -> X#queuedMessage.id =:= MessageId andalso X#queuedMessage.senderPId =:= SenderPId end,

      NewQueue = lists:filter(fun(X) -> not IsItTheMessage(X) end, Queue),
      { value, QueuedMessage } = lists:search(IsItTheMessage, Queue),
      PId ! { remove_response,  QueuedMessage},

      queue(NewQueue);

    die -> ok
  end.

% se encarga de mandar los mensajes marcados para deliver
% devuelve la lista sin los entregados
deliverDeliverable([]) -> [];
deliverDeliverable([H|T]) ->
  HeadStatus = H#queuedMessage.status,
  if
    HeadStatus =:= deliverable ->
      ledger ! H#queuedMessage.message,
      %io:format("~p~n", [H#queuedMessage.message]),
      deliverDeliverable(T);
    true -> [H|T]
  end.

% Contador especial, es actualizable con un nuevo numero,
% siempre aumenta, si el nuevo numero es mas chico se ignora
counter(Counter) ->
  receive
    { conditionalSet, NewCounter } ->
      counter(max(NewCounter, Counter));

    { getAndInc, PId } ->
      PId ! { counter_getAndInc_response, Counter+1 },
      counter(Counter+1);

    die -> ok
  end.

% Todos los nodos
allNodes() -> [node()]++nodes().

% Para debug, crea un nombre corto
shortName() ->
  X = integer_to_list(erlang:system_time('nanosecond')),
  Id = lists:sublist(X, length(X) - 10, 10),
  list_to_atom(Id).

% Para debug, hace ping a un nodo pasado como string (no atomo)
p(X) ->
  net_adm:ping(list_to_atom(X)).