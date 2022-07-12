-module(list_infer_pass).

-gradualizer(infer).
-export([f/0]).

f() ->
  V = [1, 2],
  sum(V).

-spec sum([integer()]) -> any().
sum([Int | Rest ]) -> Int + sum(Rest);
sum([]) -> 0.
