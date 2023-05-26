-module(map_refinement_fancy).

-export([map_refinement_fancy/1, just_a_bit_less_fancy/1]).

%% Expected exhaustiveness error: Values of type #{1 := 3, 2 := 4} not covered.
%%
%% Explanation: exhaustiveness checking is disabled for maps with non-singleton
%% required (exact assoc) keys.
-spec map_refinement_fancy(#{1..2 := 3..4}) -> ok.
map_refinement_fancy(#{1 := 4}) ->
    ok;
map_refinement_fancy(#{2 := 3}) ->
    ok.

%% Expected exhaustiveness error: Values #{x => a} and #{y => a} not covered.
%%
%% Explanation: exhaustiveness checking is disabled for maps with non-singleton
%% required (exact assoc) keys.
-spec just_a_bit_less_fancy(#{x|y := a}) -> ok.
just_a_bit_less_fancy(#{x := a, y := a}) -> ok.