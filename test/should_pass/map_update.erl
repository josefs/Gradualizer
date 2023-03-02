-module(map_update).

-export([mapup/2,
         mapup2/2]).

-spec mapup(boolean(), map()) -> map().
mapup(A, M1) ->
    M2 = case A of
             true ->
                 M1#{a := 1};
             false ->
                 M1#{b := 1}
         end,
    M2#{c := 1}.

%% `#{_ => _}', `#{any() => any()}', and `map()' are all equivalent.
-spec mapup2(boolean(), #{_ => _}) -> map().
mapup2(A, M1) ->
    {M2} = case A of
               true ->
                   {M1#{a := 1}};
               false ->
                   {M1#{b := 1}}
           end,
    M2#{c := 1}.
