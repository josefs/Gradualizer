-module(map_refinement_fancy).

-export([map_refinement_fancy/1]).

%% Expected exhustiveness error: Values of type #{1 := 3, 2 := 4} not covered.
%%
%% Explanation: Currently, during refinement, after clause 1 a type with
%% overlapping keys #{1..2 := 3, 2 := 3..4} is produced, which prevents
%% exhaustiveness checking. It almost works. :)
-spec map_refinement_fancy(#{1..2 := 3..4}) -> ok.
map_refinement_fancy(#{1 := 4}) ->
    ok;
map_refinement_fancy(#{2 := 3}) ->
    ok.
