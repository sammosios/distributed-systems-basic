-module(loggy).
-export([start/1, stop/1]).
-export([init/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    EmptyHoldbackQueue = [],
    loop(Clock, EmptyHoldbackQueue).

loop(Clock, HoldbackQueue) ->
    receive
        {log, From, Time, Msg} ->
            Queue1 = insert({From, Time, Msg}, HoldbackQueue),
            {Clock1, Queue2} = deliver_until_stable(Clock, Queue1),
            loop(Clock1, Queue2);
        stop ->
            io:format("loggy stopping, flushing remaining messages...~n"),
            lists:foreach(fun({F,T,M}) -> log(F, T, M) end,
                          lists:sort(fun({_,T1,_}, {_,T2,_}) -> T1 < T2 end, HoldbackQueue)),
            ok
    after 400 ->
        % {Clock1, Queue1} = deliver_until_stable(Clock, HoldbackQueue),
        % io:format("flushing queue...~n"),
            lists:foreach(fun({F,T,M}) -> log(F, T, M) end,
                          lists:sort(fun({_,T1,_}, {_,T2,_}) -> T1 < T2 end, HoldbackQueue)),
        loop(Clock, [])
    end.

%% Drain queue until no more safe messages
deliver_until_stable(Clock, Queue) ->
    case next_safe(Clock, Queue) of
        none ->
            {Clock, Queue};
        {{F,T,M}, Rest} ->
            log(F, T, M),
            Clock1 = time:update(F, T, Clock),
            deliver_until_stable(Clock1, Rest)
    end.

%% Pick the earliest candidate and check safety against current Clock
next_safe(Clock, Queue) ->
    Sorted = lists:sort(fun({_,T1,_}, {_,T2,_}) -> T1 < T2 end, Queue),
    case Sorted of
        [] -> none;
        [{F,T,M} | Rest] ->
            case time:safe({F,T}, Clock) of
                true  -> {{F,T,M}, Rest};
                false -> none
            end
    end.

%% Always keep queue sorted (by timestamp)
insert({F,T,M}, Queue) ->
    lists:sort(fun({_,T1,_}, {_,T2,_}) -> T1 < T2 end,
               [{F,T,M} | Queue]).

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
