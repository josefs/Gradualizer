-module(pattern_bind_reuse).

-export([test/2, guess_the_die/1, is_same/2]).

-spec test(integer() | undefined, integer()) -> integer().
test(I, I) -> I + I;
test(_, I) -> I.

%% Guess a number. Roll a die. If same, you win the points of the die.
-spec guess_the_die(Guess :: integer()) -> Score :: 0..6.
guess_the_die(Guess) ->
    %% Match any() against a bound variable of type integer()
    case roll_die() of
        Guess ->
            Guess; % :: integer(), but should be any() & integer()
        _WrongGuess ->
            0
    end.

%% Untyped helper; returns 1..6
roll_die() ->
    random:uniform(6).

-spec is_same(integer(), integer()) -> boolean().
is_same(N, N) ->
    true;
is_same(_, _) ->
    %% False error: This clause can't be reached
    false.
