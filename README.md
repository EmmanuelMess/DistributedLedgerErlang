# DistributedLedgerErlang
A distributer ledger with ISIS algo for total ordering of messages

[![Chat on Telegram](https://img.shields.io/badge/Chat%20on-Telegram-brightgreen.svg)](https://t.me/EmmanuelsApps)  


## Description
This has an `ledger:append` (with `{ Uid, self(), "Message" }`) that adds to the ledger and a `ledger:get` that returns the ledger. 

One ledger per node and one node per ledger (no pure-client nodes). First connect everything and then start with `ledger:start()` at each node, you can stop the ledger with `ledger:stop()`.

This implementation fails after more than n/2 nodes fail.

## Thanks

* [Formalizing and ImplementingDistributed Ledger Objects](https://arxiv.org/pdf/1802.07817.pdf) paper by Antonio Fernández Anta, Chryssis Georgiou, Kishori Konwar and Nicolas Nicolaou.
* [The explanation of the ISIS algorithm for total ordering of messages](papers/isis.pdf)
* The people giving the lectures on [Sistemas Operativos I, LCC, FCEIA, UNR 2021](https://dcc.fceia.unr.edu.ar/es/lcc/r322)
