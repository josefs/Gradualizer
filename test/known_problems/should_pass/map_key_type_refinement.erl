-module(map_key_type_refinement).

-compile([export_all, nowarn_export_all]).

-spec map_key_type_refinement(#{binary() => term()}) -> ok | error.
map_key_type_refinement(#{<<"a">> := _A}) ->
    ok;
map_key_type_refinement(#{<<"b">> := _B}) ->
    ok;
map_key_type_refinement(_Map) ->
    error.
