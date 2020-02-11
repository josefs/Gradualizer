-module(intersection_with_any).

-export([any_refined_using_guard/1,
         intersection_using_constraints/1,
         guess_two_dice/2]).

-spec any_refined_using_guard(any()) -> 5.
any_refined_using_guard(X) when is_integer(X) -> X.

-spec intersection_using_constraints(X) when X :: integer(),
                                             X :: any() ->
                                                       5.
intersection_using_constraints(X) -> X.

-spec guess_two_dice(integer(), integer()) -> 0..6.
guess_two_dice(X, Y) ->
    case roll_two_dice() of
        {X, Y} ->
            X;
        _GuessFail ->
            0
    end.

%% untyped helper returning {1..6, 1..6}
roll_two_dice() ->
    {random:uniform(6), random:uniform(6)}.
