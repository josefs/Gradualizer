-module(intersection_with_any_should_pass).

-export([guess_two_dice/2]).

-spec guess_two_dice(integer(), integer()) -> 0..6.
guess_two_dice(X, Y) ->
    case roll_two_dice() of
        {X, Y} ->
            %% "The variable X is expected to have type 0..6 but it has type integer()"
            X;
        _GuessFail ->
            0
    end.

%% untyped heper returning {1..6, 1..6}
roll_two_dice() ->
    {random:uniform(6), random:uniform(6)}.

