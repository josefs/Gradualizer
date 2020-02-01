-module(intersection_with_any).

-export([any_refined_using_guard/1,
         intersection_using_constraints/1]).

-spec any_refined_using_guard(any()) -> 5.
any_refined_using_guard(X) when is_integer(X) -> X.

-spec intersection_using_constraints(X) when X :: integer(),
                                             X :: any() ->
                                                       5.
intersection_using_constraints(X) -> X.
