-module(poly_lists_map_fail).

-gradualizer([solve_constraints]).

-export([g/1,
         h/1]).

-spec g([integer()]) -> [atom()].
g(L) ->
    lists:map(fun times_two/1, L).

-spec h([string()]) -> [integer()].
h(L) ->
	lists:map(fun times_two/1, L).

-spec times_two(integer()) -> integer().
times_two(N) -> N * 2.
