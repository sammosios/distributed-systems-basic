-module(test).
-export([run/0, run/2]).

run() ->
    DefaultSleep = 1500,
    DefaultJitter = 150,
    run(DefaultSleep, DefaultJitter).

run(Sleep, Jitter) ->
  Log = loggy:start([john, paul, ringo, george]),
  register(john, worker:start(john, Log, 13, Sleep, Jitter)),
  register(paul, worker:start(paul, Log, 23, Sleep, Jitter)),
  register(ringo, worker:start(ringo, Log, 36, Sleep, Jitter)),
  register(george, worker:start(george, Log, 49, Sleep, Jitter)),
  worker:peers(john, [paul, ringo, george]),
  worker:peers(paul, [john, ringo, george]),
  worker:peers(ringo, [john, paul, george]),
  worker:peers(george, [john, paul, ringo]),
  timer:sleep(5000),
  loggy:stop(Log),
  worker:stop(john),
  worker:stop(paul),
  worker:stop(ringo),
  worker:stop(george).