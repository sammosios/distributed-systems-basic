-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Loggerger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Loggerger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Logger, Seed, Sleep, Jitter) ->
    rand:seed(exsplus, {Seed, Seed, Seed}),
    receive
        {peers, Peers} ->
            %% Extract all peer names + self
            Names = [Name | Peers],
            % io:format("~p knows peers ~p~n", [Name, Peers]),
            FullClock = time:clock(Names),
            loop(Name, Logger, FullClock, Peers, Sleep, Jitter);
        stop ->
            ok
    end.

peers(Worker, Peers) ->
  Worker ! {peers, Peers}.

loop(Name, Logger, Time, Peers, Sleep, Jitter) ->
  Wait = rand:uniform(Sleep),
  receive
    {msg, MsgTime, Msg} ->
      Time1 = time:inc(Name, time:merge(Time, MsgTime)),
      Logger ! {log, Name, Time1, {received, Msg}},
      loop(Name, Logger, Time1, Peers, Sleep, Jitter);
    stop ->
      ok;
    Error ->
      Logger ! {log, Name, Time, {error, Error}}
  after Wait ->
    Selected = select(Peers),
    Time1 = time:inc(Name, Time),
    Message = {hello, rand:uniform(100)},
    Selected ! {msg, Time1, Message},
    jitter(Jitter),
    Logger ! {log, Name, Time1, {sending, Message}},
    loop(Name, Logger, Time1, Peers, Sleep, Jitter)
  end.

select(Peers) ->
  lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;

jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).