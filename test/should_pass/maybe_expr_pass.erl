-module(maybe_expr_pass).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 25).
-if(?FEATURE_AVAILABLE(maybe_expr)).

-feature(maybe_expr, enable).

-export([check1/0, check2/0]).
-export([infer1/0, infer2/1]).

-spec check1() -> integer().
check1() ->
    maybe
        ok ?= ok,
        1
    end.

-spec check2() -> integer().
check2() ->
    maybe
        ok ?= not_ok,
        1
    else
        _ -> 2
    end.

-spec infer1() -> integer().
infer1() ->
    R = maybe
        ok ?= ok,
        1
    end,
    R.

-spec infer2(string()) -> {ok, string()} | error.
infer2(Val) ->
    R = maybe
        "ok" ?= Val,
        {ok, Val}
    else
        _ -> error
    end,
    R.

-endif. %% FEATURE_AVAILABLE
-endif. %% OTP >= 25
-endif. %% OTP_RELEASE
