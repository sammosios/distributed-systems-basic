-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false -> 
            notfound;
        {Name, _Ref, Pid} -> 
            {ok, Pid}
    end.

ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false -> 
            notfound;
        {Name, Ref, _Pid} -> 
            {ok, Ref}
    end.

name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        false -> 
            notfound;
        {Name, Ref, _Pid} -> 
            {ok, Name}
    end.

list(Intf) ->
    lists:map(fun({Name, _Ref, _Pid}) -> Name end, Intf).

broadcast(Message, Intf) ->
    lists:foreach(
      fun({_Name, _Ref, Pid}) -> 
              Pid ! Message 
      end,
      Intf
    ).