-module(ring).
-export([init/1, add_nodes/1, remote_add_nodes/2, data_random/1]).

-define(Module, node2).

init(N) ->
  test:ring(?Module, N).

add_nodes(N) ->
  test:start(?Module, N, base).

remote_add_nodes(N, RHost) ->
  test:start(?Module, N, {base, RHost}).

data_random(N) ->
  Keys = test:random_add(N, base),
  Keys.
