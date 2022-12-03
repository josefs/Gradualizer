-module(poly_lists_map_should_fail).

-gradualizer([solve_constraints]).

-export([i/1,
         j/1]).

-spec i([binary() | integer()]) -> [integer()].
i(L) ->
    lists:map(fun
                  (I) when is_integer(I) -> I * 2;
                  (B) when is_list(B) -> list_to_integer(B)
              end, L).

-spec j([binary() | integer()]) -> [integer()].
j(L) ->
    lists:map(fun
                  (I) when is_integer(I) -> I * 2
              end, L).
