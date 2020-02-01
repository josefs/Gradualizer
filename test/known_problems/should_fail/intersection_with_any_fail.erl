-module(intersection_with_any).

-export([any_refined_using_guard/1,
         intersection_using_constraits/1]).

%% X :: any() & atom() by refinement
-spec any_refined_using_guard(any()) -> 5.
any_refined_using_guard(X) when is_atom(X) ->
    X.

%% X :: integer() & any() by the constraints
-spec intersection_using_constraits(X) when X :: integer(),
                                            X :: any() ->
                                                      atom().
intersection_using_constraits(X) ->
    X.
