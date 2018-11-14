-module(named_fun).

-export([fac/1]).

-spec fac(integer()) -> integer().
fac(I) ->
    F = fun Fac(0) ->
                1;
            Fac(N) ->
                N*Fac(N-1)
        end,
    F(I).
