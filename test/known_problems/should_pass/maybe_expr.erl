-module(maybe_expr).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 25).
-if(?FEATURE_AVAILABLE(maybe_expr)).

-feature(maybe_expr, enable).

-export([syntax/0]).

syntax() ->
    maybe
        ok ?= ok
    end.

-endif. %% FEATURE_AVAILABLE
-endif. %% OTP >= 25
-endif. %% OTP_RELEASE
