-module(poly_should_fail).

-gradualizer([solve_constraints]).

-export([f/1]).

-spec f([integer()]) -> [atom()].
f(L) ->
    lists:map(fun helper/1, L).

-spec helper(integer()) -> integer().
helper(I) -> I * 2.
