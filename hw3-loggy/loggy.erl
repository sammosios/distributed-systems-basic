-module(loggy).
-export([start/1, stop/1]).

start(Names) ->
    spawn_link(fun() -> init(Names) end).

stop(Logger) ->
    Logger ! stop.

init(Names) ->
    Clock = time:clock(Names),
    loop(Clock, []).

loop(Clock, Holdback) ->
    receive
        {log, Name, MsgTime, Event} ->
            %% Add new event to holdback
            Holdback1 = [{Name, MsgTime, Event} | Holdback],
            {Ready, Rest} = partition_safe(Holdback1, Clock),
            Clock1 = lists:foldl(fun({_,T,_},Acc) -> time:merge(Acc,T) end, Clock, Ready),
            lists:foreach(fun({N,T,E}) ->
                              io:format("~p ~p ~p~n",[N,T,E])
                          end, Ready),
            loop(Clock1, Rest);

        stop ->
            io:format("loggy stopping, flushing remaining messages...~n"),
            flush(Holdback, Clock)
    end.

flush([], _) -> ok;
flush(Holdback, Clock) ->
    {Ready, Rest} = partition_safe(Holdback, Clock),
    Clock1 = lists:foldl(fun({_,T,_},Acc) -> time:merge(Acc,T) end, Clock, Ready),
    lists:foreach(fun({N,T,E}) ->
                      io:format("~p ~p ~p~n",[N,T,E])
                  end, Ready),
    flush(Rest, Clock1).

%% Split queue into safe vs not-yet-safe
partition_safe(Holdback, Clock) ->
    lists:partition(
      fun({Sender, MsgTime, _}) -> time:safe(Sender, MsgTime, Clock) end,
      Holdback).
