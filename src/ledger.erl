-module(ledger).

-export([start/0, finish/0, get/0, pending/1, ledgerNode/1, counter/1, append/1, killer/1]).

% Inicia el ledger y el modulo isis
start() ->
  isis:start(),
  register(ledger, spawn(?MODULE, ledgerNode, [[]])),
  register(counterMaintainer, spawn(?MODULE, counter, [0])),
  register(getPendingMaintainer, spawn(?MODULE, pending, [sets:new()])),
  register(appendPendingMaintainer, spawn(?MODULE, pending, [sets:new()])),
  N = length(allNodes()),
  register(checkLivenessCondition, spawn(?MODULE, killer, [N])),
  checkLivenessCondition ! { check, self() },
  receive check_response -> ok end.

% Finaliza el ledger y el submecanismo de isis
finish() ->
  ledger ! die,
  counterMaintainer ! die,
  getPendingMaintainer ! die,
  appendPendingMaintainer ! die,
  unregister(ledger),
  unregister(counterMaintainer),
  unregister(getPendingMaintainer),
  unregister(appendPendingMaintainer),
  isis:finish().

ledgerNode(Ledger) ->
  checkLivenessCondition ! { check, self() },
  receive check_response -> ok end,

  ledgerNodeGet(Ledger),

  checkLivenessCondition ! { check, self() },
  receive check_response -> ok end,

  NewLedger = ledgerNodeAppend(Ledger),
  receive
    die -> ok
  after 0 ->
    ledgerNode(NewLedger)
  end.

% Recive y procesa los get de los clientes
ledgerNodeGet(Ledger) ->
  receive
    { client_get, PId, Counter } ->
      isis:broadcast({ server_get, PId, Counter }),
      getPendingMaintainer ! { add, PId, Counter };
    { server_get, PId, Counter } ->
      getPendingMaintainer ! { has, self(), PId, Counter },
      IsPending = receive { isPending, I } -> I end,
      if
        IsPending ->
          PId ! { get_response, self(), Ledger, Counter },
          getPendingMaintainer ! { remove, PId, Counter };
        true -> ok
      end
  after
    0 -> ok % No bloqueante
  end,
  Ledger.

% Recive y procesa los append de los clientes
ledgerNodeAppend(Ledger) ->
  NewLedger =
    receive
      { client_append, PId, Entry, Counter } ->
        isis:broadcast({ server_append, PId, Entry, Counter }),
        appendPendingMaintainer ! { add, Entry, Counter },
        Ledger;
      { server_append, PId, Entry, Counter } ->
        IsNotInLedger = not lists:member(Entry, Ledger),
        if
          IsNotInLedger ->
            L = Ledger ++ [Entry],
            appendPendingMaintainer ! { has, self(), Entry, Counter },
            IsPending = receive { isPending, I } -> I end,
            if
              IsPending ->
                PId ! { append_response, ok, Counter },
                appendPendingMaintainer ! { remove, Entry, Counter };
              true -> ok
            end,
            L;
          true ->
            PId ! { append_response, already_present, Counter },
            Ledger
        end
    after
      0 -> Ledger % No bloqueante
    end,
  NewLedger.

% Mantinene una lista de pendientes
pending(Pending) ->
  receive
    { add, Request, Counter } ->
      NewPending = sets:add_element({ Request, Counter }, Pending),
      pending(NewPending);
    { has, PId, Request, Counter } ->
      PId ! { isPending, sets:is_element({ Request, Counter }, Pending) },
      pending(Pending);
    { remove, Request, Counter } ->
      NewPending = sets:del_element({ Request, Counter }, Pending),
      pending(NewPending);
    die -> ok
  end.


% Agrega al ledger
append(Record) ->
  { Uid, Sender, Message } = Record, % Esto es para denotar la forma de Record
  counterMaintainer ! { get, self() },
  Counter = receive { counter, C } -> C end,
  lists:foreach(fun(Node) -> { ledger, Node } ! { client_append, self(), Record, Counter } end, allNodes()),
  Response = receive { append_response, R, Counter } -> R end,
  emptyInbox(),
  Response.

% Obtiene el ledger
get() ->
  counterMaintainer ! { get, self() },
  Counter = receive { counter, C } -> C end,
  lists:foreach(fun(Node) -> { ledger, Node } ! { client_get, self(), Counter } end, allNodes()),
  LedgerObject = receive { get_response, _, L, Counter } -> L end,
  emptyInbox(),
  LedgerObject.

% Mata todo cuando se deja de cumplir la condicion de nodosVivos >= f + 1 con f = n / 2
killer(N) ->
  ThreshHold = (N / 2) + 1,
  CurrentAlive = length(allNodes()),
  receive
    { check, PId } ->
      if
        CurrentAlive < ThreshHold ->
          io:format("Se perdio la garantia de nodos vivos (se cayo mas de la mitad de la red)!~n"),
          finish();
        true ->
          PId ! check_response,
          killer(N)
      end
  end.

% Contador simple y monotonicamente creciente
counter(C) ->
  receive
    { get, PId } ->
      PId ! { counter, C },
      counter(C + 1);
    die -> ok
  end.

% Vacia la casilla de entrada, para evitar spam
emptyInbox() ->
  receive
    _ -> emptyInbox()
  after
    0 -> ok
  end.

% Todos los nodos, incluye el actual
allNodes() -> [node()]++nodes().