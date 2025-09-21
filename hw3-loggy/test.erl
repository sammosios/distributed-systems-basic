-module(test).
-export([run/0, run/2]).

run() ->
    DefaultSleep = 1500,
    DefaultJitter = 1910,
    run(DefaultSleep, DefaultJitter).

run(Sleep, Jitter) ->
  Log = loggy:start([john, paul, ringo, george]),
  A = worker:start(john, Log, 13, Sleep, Jitter),
  B = worker:start(paul, Log, 23, Sleep, Jitter),
  C = worker:start(ringo, Log, 36, Sleep, Jitter),
  D = worker:start(george, Log, 49, Sleep, Jitter),
  worker:peers(A, [B, C, D]),
  worker:peers(B, [A, C, D]),
  worker:peers(C, [A, B, D]),
  worker:peers(D, [A, B, C]),
  timer:sleep(5000),
  loggy:stop(Log),
  worker:stop(A),
  worker:stop(B),
  worker:stop(C),
  worker:stop(D).