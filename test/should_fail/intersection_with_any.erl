-module(intersection_with_any).

-export([
    intersection_using_constraints/1,
    intersection_using_constraints2/1]).

%% X :: integer() & any() by the constraints
-spec intersection_using_constraints(X) -> 5 when X :: integer(), X :: any().
intersection_using_constraints(X) ->
    X.

-spec intersection_using_constraints2(X) -> atom() when X :: integer(), X :: any() .
intersection_using_constraints2(X) ->
    X.
