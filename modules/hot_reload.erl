-module(hot_reload).
-export([reload/3]).

reload(ModuleName, Entrypoint, RegisteredName) ->
    %% Convert atom to filename string
    FileName = atom_to_list(ModuleName) ++ ".erl",

    %% Save old PID if exists
    OldPid = case whereis(RegisteredName) of
        undefined -> undefined;
        Pid -> Pid
    end,

    %% Try to compile the module
    case compile:file(FileName, [load]) of
        {ok, _Module} ->
            %% Purge old version and reload
            code:purge(ModuleName),
            code:load_file(ModuleName),
            stop_old_process(OldPid, RegisteredName),
            spawn_and_register(ModuleName, Entrypoint, RegisteredName);  % <- return directly

        Error ->
            io:format("Failed to compile ~p: ~p~n", [ModuleName, Error]),
            {error, Error}
    end.

%% Helper to stop old process
stop_old_process(undefined, _Name) -> ok;
stop_old_process(Pid, Name) ->
    exit(Pid, kill),
    unregister(Name).

%% Spawn and register safely
spawn_and_register(ModuleName, Entrypoint, RegisteredName) ->
    Pid = spawn(ModuleName, Entrypoint, []),
    register(RegisteredName, Pid),
    io:format("\nModule: ~p\nFunction: ~p\nPid: ~p\nRegistered as: ~p~n~n",
              [ModuleName, Entrypoint, Pid, RegisteredName]),
    {ok, Pid}.
