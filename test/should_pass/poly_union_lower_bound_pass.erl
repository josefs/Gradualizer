-module(poly_union_lower_bound_pass).

-gradualizer([solve_constraints]).

-export([i/1]).

-spec i([binary() | integer()]) -> [integer()].
i(L) ->
    lists:map(fun takes_a_union/1, L).

-spec takes_a_union(binary() | integer()) -> integer().
takes_a_union(B) when is_binary(B) -> binary_to_integer(B);
takes_a_union(I) when is_integer(I) -> I.
