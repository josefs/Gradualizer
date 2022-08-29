-module(list_infer_fail).

-gradualizer(infer).
-export([f/0]).

f() ->
  V = [1, 2],
  g(V).

-spec g(integer()) -> any().
g(Int) -> Int + 1.
