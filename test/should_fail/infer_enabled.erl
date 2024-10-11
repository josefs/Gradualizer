-module(infer_enabled).

-export([f/0]).

f() ->
    X = 1, Y = banana,
    X + Y.
