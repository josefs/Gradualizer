-module(map_update1).

-export([mapup/2]).

-spec mapup(boolean(), map()) -> map().
mapup(A, M1) ->
    M2 = case A of
             true ->
                 M1#{a := 1};
             false ->
                 M1#{b := 1}
         end,
    M2#{c := 1}.
