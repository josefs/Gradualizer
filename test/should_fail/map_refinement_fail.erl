-module(map_refinement_fail).

-compile([export_all, nowarn_export_all]).

%% Expected error: Nonexhaustive patterns. Empty map not covered.
-spec map_value_type_refinement(#{x => a|b}) -> ok.
map_value_type_refinement(#{x := a}) ->
    ok;
map_value_type_refinement(#{x := b}) ->
    ok.
