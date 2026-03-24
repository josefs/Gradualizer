-module(strict_map_comprehension_pass).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 28).

-compile([export_all, nowarn_export_all]).

%% Strict map generator
-spec strict_mc(#{atom() => integer()}) -> #{atom() => integer()}.
strict_mc(M) ->
    #{K => V + 1 || K := V <:- M}.

%% Strict map generator in list comprehension
-spec strict_map_gen_lc(#{atom() => integer()}) -> [integer()].
strict_map_gen_lc(M) ->
    [V || _K := V <:- M].

-endif. %% OTP >= 28
-endif. %% OTP_RELEASE
