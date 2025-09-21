-module(time).
-export([zero/0, inc/2, merge/2, leq/2]).
-export([clock/1, update/3, safe/2]).

zero() ->
    0.

inc(_Name, T) ->
    T + 1.

merge(Ti, Tj) ->
    max(Ti, Tj).

leq(Ti, Tj) ->
    Ti =< Tj.

%% A simple Lamport clock is just an integer, not a list of nodes.
clock(_Nodes) ->
    zero().

%% The update function for a simple Lamport clock is just merge.
update(_Node, Time, Clock) ->
    merge(Time, Clock).

%% A message is safe to deliver if the local clock is greater than or equal to the message timestamp.
safe({_Sender, Time}, Clock) ->
    Clock >= Time.

