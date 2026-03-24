-module(sigils_fail).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 27).

%% Each exported function should produce exactly one type error.

-export([sigil_wrong_type/0]).

%% ~"..." produces a binary, not an integer
-spec sigil_wrong_type() -> integer().
sigil_wrong_type() ->
    ~"hello".

-endif. %% OTP >= 27
-endif. %% OTP_RELEASE
