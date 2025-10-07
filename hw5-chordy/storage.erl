-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
  maps:new().

add(Key, Value, Store) ->
  maps:put(Key, Value, Store).

lookup(Key, Store) ->
  case maps:find(Key, Store) of
    {ok, Value} -> {Key, Value};
    error -> false
  end.

split(From, To, Store) ->
    {In, Out} = lists:partition(
                    %% Convert Store to list, partition to {In, Out} depending on key:between
                   fun({K, _V}) -> key:between(K, From, To) end,
                   maps:to_list(Store)
                 ),
    {maps:from_list(In), maps:from_list(Out)}.


merge(Entries, Store) ->
  % Not sure what merging strategy is best when dealing with same-key conflicts
  % For now I will just preserve the values in Store
  maps:merge(Entries, Store).
