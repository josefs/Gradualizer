-module(map_comprehension_fail).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 27).

%% Each exported function should produce exactly one type error.

-export([mc_wrong_return_type/0, map_gen_not_map/1]).

%% Map comprehension result used where integer is expected
-spec mc_wrong_return_type() -> integer().
mc_wrong_return_type() ->
    #{X => X || X <- [1, 2, 3]}.

%% Map generator from non-map value
-spec map_gen_not_map(integer()) -> [integer()].
map_gen_not_map(N) ->
    [V || _K := V <- N].

-endif. %% OTP >= 27
-endif. %% OTP_RELEASE
