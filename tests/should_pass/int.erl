-module(int).

-compile([export_all]).

-spec f(integer()) -> integer().
f(N) ->
    N.
