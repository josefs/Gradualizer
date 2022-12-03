-module(poly_lists_map_pass).

-gradualizer([solve_constraints]).

-export([f/1,
         k/1]).

-spec f([integer()]) -> [integer()].
f(L) ->
    lists:map(fun (I) -> I * 2 end, L).

-spec k([binary() | integer()]) -> [integer()].
k(L) ->
    lists:map(fun
                  (I) when is_integer(I) -> I * 2;
                  (B) when is_binary(B) -> binary_to_integer(B)
              end, L).
