-module(intersection_with_any_fail).

-export([intersection_using_constraints/1]).

-spec intersection_using_constraints(X) -> atom() when X :: integer(), X :: any() .
intersection_using_constraints(X) ->
    X.
