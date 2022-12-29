-module(poly_should_fail).

-gradualizer([solve_constraints]).

-export([f/1,
         g/1]).

-spec f([integer()]) -> [atom()].
f(L) ->
    lists:map(fun helper/1, L).

-spec helper(integer()) -> integer().
helper(I) -> I * 2.

-spec g(integer()) -> atom().
g(I) ->
    app(fun helper/1, I).

-spec app(fun ((A) -> B), A) -> B.
app(F, A) ->
    F(A).
