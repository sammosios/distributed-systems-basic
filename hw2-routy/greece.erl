-module(greece).
-export([init/0]).

init() ->
    %% Start all routers
    RoutNames = [ath, skg, lrs, crt, ioa],
    Pids = [{Name, routy:start(Name, Name)} || Name <- RoutNames],

    %% Build a mapping Name -> PID for convenience
    NamePidMap = lists:foldl(fun({Name, Pid}, Acc) -> [{Name, Pid} | Acc] end, [], Pids),

    %% For each router, send {add, Node, Pid} for all other routers
    lists:foreach(
        fun({Name, Pid}) ->
            OtherRouters = lists:filter(fun({OtherName, _}) -> OtherName =/= Name end, NamePidMap),
            lists:foreach(
                fun({OtherName, OtherPid}) ->
                    Pid ! {add, OtherName, OtherPid}
                end,
                OtherRouters
            )
        end,
        Pids
    ),

    io:format("All routers started and connected in a full mesh.~n").
