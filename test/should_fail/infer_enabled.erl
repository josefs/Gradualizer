-module(infer_enabled).

-gradualizer(infer).
-export([f/0]).

f() ->
    X = 1, Y = banana,
    X + Y.
