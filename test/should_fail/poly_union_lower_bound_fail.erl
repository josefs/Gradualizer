-module(poly_union_lower_bound_fail).

-gradualizer([solve_constraints]).

-export([i/1]).

-spec i([binary() | integer()]) -> [integer()].
i(L) ->
    lists:map(fun erlang:integer_to_list/1, L).
