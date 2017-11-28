-module(int).

-compile([export_all]).

-spec f(int()) -> int().
f(N) ->
    N.
