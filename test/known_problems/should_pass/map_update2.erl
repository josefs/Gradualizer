-module(map_update2).

-export([mapup2/2]).

%% `#{_ => _}', `#{any() => any()}', and `map()' are all equivalent.
-spec mapup2(boolean(), #{_ => _}) -> map().
mapup2(A, M1) ->
    {M2} = case A of
               true ->
                   {M1#{a := 1}};
               false ->
                   {M1#{b := 1}}
           end,
    %% inferred type of M2 is none()
    %% because glb_ty cannot handle maps with overlapping keys

    %% update_map_type does not handle none()
    M2#{c := 1}.
