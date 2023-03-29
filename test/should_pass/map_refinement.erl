-module(map_refinement).

-compile([export_all, nowarn_export_all]).

-spec map_key_type_refinement(#{binary() => term()}) -> ok | error.
map_key_type_refinement(#{<<"a">> := _A}) ->
    ok;
map_key_type_refinement(#{<<"b">> := _B}) ->
    ok;
map_key_type_refinement(_Map) ->
    error.

-spec map_value_type_refinement(#{x => a|b}) -> ok.
map_value_type_refinement(#{x := a}) ->
    ok;
map_value_type_refinement(#{x := b}) ->
    ok;
map_value_type_refinement(#{}) ->
    ok.

-spec optional_key(#{x := a, y => b}) -> ok.
optional_key(#{x := a, y := b}) -> ok;
optional_key(#{x := a}) -> ok.

-spec optional_key_2(#{x := a, y => b}) -> ok.
optional_key_2(#{x := a}) -> ok.

-spec catch_all_map_pattern(#{x := a|b, y => c|d|e, z := integer()}) -> ok.
catch_all_map_pattern(#{}) -> ok.

-spec any_map(map()) -> ok.
any_map(#{x := a}) -> ok;
any_map(#{y := b}) -> ok.

-spec multiple_values(#{x := a|b, y := c|d}) -> ok.
multiple_values(#{x := a, y := c}) -> ok;
multiple_values(#{x := a, y := d}) -> ok;
multiple_values(#{x := b, y := d}) -> ok;
multiple_values(#{x := b, y := c}) -> ok.

-spec multiple_keys(#{x|y => a}) -> ok.
multiple_keys(#{x := a, y := a}) -> ok;
multiple_keys(#{x := a}) -> ok;
multiple_keys(#{y := a}) -> ok;
multiple_keys(#{}) -> ok.

-spec multiple_keys_and_values(#{x|y => a|b}) -> ok.
multiple_keys_and_values(#{x := a, y := a}) -> ok;
multiple_keys_and_values(#{x := a, y := b}) -> ok;
multiple_keys_and_values(#{x := b, y := a}) -> ok;
multiple_keys_and_values(#{x := b, y := b}) -> ok;
multiple_keys_and_values(#{x := a}) -> ok;
multiple_keys_and_values(#{x := b}) -> ok;
multiple_keys_and_values(#{}) -> ok.

-spec infinite_value_type(#{x := integer()}) -> integer().
infinite_value_type(#{x := 1}) -> 2;
infinite_value_type(#{x := 2}) -> 3;
infinite_value_type(#{x := N}) -> N + 1.

-spec infinite_key_type(#{integer() => true}) -> ok.
infinite_key_type(#{1 := true}) -> ok;
infinite_key_type(#{2 := true}) -> ok;
infinite_key_type(_Map) -> ok.

-spec union_of_maps(#{x := a} | #{x := b}) -> ok.
union_of_maps(#{x := a}) -> ok;
union_of_maps(#{x := b}) -> ok.

-spec union_of_maps_2(#{x := a} | #{y := b}) -> ok.
union_of_maps_2(#{x := a}) -> ok;
union_of_maps_2(#{y := b}) -> ok.

-spec union_of_maps_3(#{x := a} | #{x := a|b}) -> ok.
union_of_maps_3(#{x := a}) -> ok;
union_of_maps_3(#{x := b}) -> ok.
