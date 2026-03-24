-module(strict_generators_fail).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 28).

%% Each exported function should produce exactly one type error.

-export([strict_gen_not_list/1, strict_gen_wrong_type/0]).

%% Strict list generator from non-list (integer)
-spec strict_gen_not_list(integer()) -> list().
strict_gen_not_list(N) ->
    [X || X <:- N].

%% Strict list generator result type mismatch
-spec strict_gen_wrong_type() -> integer().
strict_gen_wrong_type() ->
    [X || X <:- [1, 2, 3]].

-endif. %% OTP >= 28
-endif. %% OTP_RELEASE
