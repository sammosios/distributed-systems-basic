-module(client).
-export([ping/0]).

ping() ->
    Self = self(),
    Module_Name = wait,
    Target = 'foo@130.229.144.115',
    {Module_Name, Target} ! {"ClientHello", Self},
    receive
        Msg -> io:format("Got reply: ~p~n", [Msg])
    after 5000 ->
        io:format("No reply received within timeout~n")
    end.
