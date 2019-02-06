-module(map_update).

-export([mapup/2,
         mapup2/2
        ]).

-spec mapup(boolean(), map()) -> map().
mapup(A, M1) ->
    M2 = case A of
             true ->
                 M1#{a := 1};
             false ->
                 M1#{b := 1}
         end,
    %% function_clause in update_map_type
    %% which does not handle union of maps
    M2#{c := 1}.

-spec mapup2(boolean(), map()) -> map().
mapup2(A, M1) ->
    {M2} = case A of
               true ->
                   {M1#{a := 1}};
               false ->
                   {M1#{b := 1}}
           end,
    %% inferred type of M2 is none()
    %% because glb_ty gives up on non-trivial map types

    %% update_map_type does not handle none()
    M2#{c := 1}.
