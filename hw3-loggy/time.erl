-module(time).
-export([zero/0, clock/1, inc/2, merge/2, leq/2, safe/3]).

%% Create an empty clock
zero() ->
    #{}.

%% Initialize a clock with all nodes set to 0
clock(Names) ->
    maps:from_list([{N, 0} || N <- Names]).

%% Increment the clock for a given Name
inc(Name, Clock) ->
    maps:update_with(Name, fun(V) -> V + 1 end, 1, Clock).

%% Merge two clocks: take max per entry
merge(C1, C2) ->
    maps:fold(
      fun(K, V2, Acc) ->
          V1 = maps:get(K, Acc, 0),
          maps:put(K, max(V1, V2), Acc)
      end,
      C1,
      C2).

%% True if C1 <= C2 element-wise
leq(C1, C2) ->
    lists:all(
      fun(Name) ->
          maps:get(Name, C1, 0) =< maps:get(Name, C2, 0)
      end,
      lists:usort(maps:keys(C1) ++ maps:keys(C2))
    ).

%% Safe delivery check: 
%% For sender’s entry: MsgVal =< LocalVal + 1
%% For all others:     MsgVal =< LocalVal
safe(Sender, MsgTime, LocalTime) ->
    lists:all(
      fun(Name) ->
          MsgVal = maps:get(Name, MsgTime, 0),
          LocalVal = maps:get(Name, LocalTime, 0),
          if Name =:= Sender ->
                 MsgVal =< LocalVal + 1;
             true ->
                 MsgVal =< LocalVal
          end
      end,
      lists:usort(maps:keys(MsgTime) ++ maps:keys(LocalTime))
    ).
