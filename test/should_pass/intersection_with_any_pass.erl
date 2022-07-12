-module(intersection_with_any_pass).

-compile([export_all, nowarn_export_all]).

-spec any_refined_using_guard(any()) -> 5.
any_refined_using_guard(X) when is_integer(X) -> X.

-spec intersection_using_constraints(X) -> X when
                                                X :: integer(),
                                                X :: any().
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
    {rand:uniform(6), rand:uniform(6)}.
