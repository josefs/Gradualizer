-module(intersection_with_any).

-export([intersection_using_constraints/1]).

%% X :: integer() & any() by the constraints
-spec intersection_using_constraints(X) -> 5 when X :: integer(), X :: any().
intersection_using_constraints(X) ->
    % X :: integer() & any() at this point,
    % which should be compatible with X :: 5
    X.
