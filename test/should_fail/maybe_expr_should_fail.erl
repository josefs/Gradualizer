-module(maybe_expr_should_fail).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 25).
-if(?FEATURE_AVAILABLE(maybe_expr)).

-feature(maybe_expr, enable).

-export([check1/0, check2/0]).
-export([infer1/0, infer2/1]).
-export([wrong_body_type/0, wrong_else_type/1]).

-spec check1() -> integer().
check1() ->
    maybe
        ok ?= ok,
        "one"
    end.

-spec check2() -> integer().
check2() ->
    maybe
        ok ?= not_ok,
        one
    else
        _ -> "two"
    end.

-spec infer1() -> integer().
infer1() ->
    R = maybe
        ok ?= ok
    end,
    R.

-spec infer2(string()) -> integer().
infer2(Val) ->
    R = maybe
        ok ?= Val
    else
        _ -> ok
    end,
    R.

%% Body expression has wrong type (string instead of integer)
-spec wrong_body_type() -> integer().
wrong_body_type() ->
    maybe
        ok ?= ok,
        "hello"
    end.

%% Else clause returns wrong type
-spec wrong_else_type({ok, integer()} | error) -> integer().
wrong_else_type(Input) ->
    maybe
        {ok, Val} ?= Input,
        Val
    else
        error -> "not a number"
    end.

-endif. %% FEATURE_AVAILABLE
-endif. %% OTP >= 25
-endif. %% OTP_RELEASE
