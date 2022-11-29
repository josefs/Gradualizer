-module(maybe_expr).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 25).
-if(?FEATURE_ENABLED(maybe_expr)).

-export([syntax/0]).

syntax() ->
    maybe
        ok ?= ok
    end.

-endif. %% FEATURE_ENABLED
-endif. %% OTP >= 25
-endif. %% OTP_RELEASE
