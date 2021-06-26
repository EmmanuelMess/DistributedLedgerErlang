# DistributedLedgerErlang
A distributer ledger with ISIS algo for total ordering of messages

[![Chat on Telegram](https://img.shields.io/badge/Chat%20on-Telegram-brightgreen.svg)](https://t.me/EmmanuelsApps)  


## Description
This has an `ledger:append` (with `{ Uid, self(), "Message" }`) that adds to the ledger and a `ledger:get` that returns the ledger. 

One ledger per node and one node per ledger (no pure-client nodes).

This implementation fails after more than n/2 nodes fail.

## Running

Compile everything:

```sh
erl -compile isis ledger
```

Run this on each terminal (one per node):

```sh
erl -setcookie wermer -snode <nodename>
net_adm:ping('<othernodename>@<host>').
```

Connect everything and then start with `ledger:start()` at each node, you can stop the ledger with `ledger:stop()`:

```erl
ledger:start().
```

Then you can use the ledger with `ledger:append` and `ledger:get()`:

```erl
> ledger:get().
[]

> ledger:append({0, self(), "Hola"}).

> ledger:get().
[{0, <0.72.0>, "Hola"}]
```

## Thanks

* [Formalizing and ImplementingDistributed Ledger Objects](https://arxiv.org/pdf/1802.07817.pdf) paper by Antonio Fernández Anta, Chryssis Georgiou, Kishori Konwar and Nicolas Nicolaou.
* [The explanation of the ISIS algorithm for total ordering of messages](papers/isis.pdf)
* The people giving the lectures on [Sistemas Operativos I, LCC, FCEIA, UNR 2021](https://dcc.fceia.unr.edu.ar/es/lcc/r322)
