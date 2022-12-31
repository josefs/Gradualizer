-module(poly_lists_map_fail).

-gradualizer([solve_constraints]).

-export([g/1,
         h/1]).

-spec g([integer()]) -> [atom()].
g(L) ->
    lists:map(fun (I) -> I * 2 end, L).

-spec h([string()]) -> [integer()].
h(L) ->
	lists:map(fun (I) -> I * 2 end, L).
