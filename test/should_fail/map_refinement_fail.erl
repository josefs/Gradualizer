-module(map_refinement_fail).

-compile([export_all, nowarn_export_all]).

%% Expected error: Nonexhaustive patterns. Empty map not covered.
-spec map_value_type_refinement(#{x => a|b}) -> ok.
map_value_type_refinement(#{x := a}) ->
    ok;
map_value_type_refinement(#{x := b}) ->
    ok.

% Expected error: Nonexhaustive patterns. #{x => a} not covered.
-spec optional_key(#{x := a, y => b}) -> ok.
optional_key(#{x := a, y := b}) -> ok.

% Expected error: Nonexhaustive patterns. #{x => b, y => c} not covered.
-spec multiple_values(#{x := a|b, y := c|d}) -> ok.
multiple_values(#{x := a, y := c}) -> ok;
multiple_values(#{x := a, y := d}) -> ok;
multiple_values(#{x := b, y := d}) -> ok.

%% Expected error: Nonexhaustive patterns. Empty map not covered.
-spec multiple_keys(#{x|y => a}) -> ok.
multiple_keys(#{x := a, y := a}) -> ok;
multiple_keys(#{x := a}) -> ok;
multiple_keys(#{y := a}) -> ok.

% Expected error: Nonexhaustive patterns. #{x => b} (and others) not covered.
-spec multiple_keys_and_values(#{x|y => a|b}) -> ok.
multiple_keys_and_values(#{x := a, y := a}) -> ok;
multiple_keys_and_values(#{x := a, y := b}) -> ok;
multiple_keys_and_values(#{x := b, y := a}) -> ok;
multiple_keys_and_values(#{x := b, y := b}) -> ok;
multiple_keys_and_values(#{x := a}) -> ok.

%% Expected error: Invalid key. (The pattern #{x => a, y => a) has
%% already been covered by the first clause.)
-spec multiple_keys_2(#{x|y => a}) -> ok.
multiple_keys_2(#{x := a}) -> ok;
multiple_keys_2(#{y := a}) -> ok;
multiple_keys_2(#{x := a, y := a}) -> ok.

% Expected error: Nonexhaustive patterns. #{x => 0} not covered.
-spec infinite_value_type(#{x := integer()}) -> ok.
infinite_value_type(#{x := 1}) -> ok;
infinite_value_type(#{x := 2}) -> ok.

% Expected error: Nonexhaustive patterns. #{0 => true} not covered.
-spec infinite_key_type(#{integer() => true}) -> ok.
infinite_key_type(#{1 := true}) -> ok;
infinite_key_type(#{2 := true}) -> ok.

% Expected error: Nonexhaustive patterns. #{x => b} not covered.
-spec union_of_maps(#{x := a} | #{x := b}) -> ok.
union_of_maps(#{x := a}) -> ok.

% Expected error: Nonexhaustive patterns. #{y => b} not covered.
-spec union_of_maps_2(#{x := a} | #{y := b}) -> ok.
union_of_maps_2(#{x := a}) -> ok.

% Expected error: Nonexhaustive patterns. #{x => b} not covered.
-spec union_of_maps_3(#{x := a} | #{x := a|b}) -> ok.
union_of_maps_3(#{x := a}) -> ok.
