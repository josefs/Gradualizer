-module(cyclic_otp_specs).

-compile([export_all, nowarn_export_all]).

%% We shouldn't fail on cyclic dependencies in OTP specs.
%% Either make them work, or override the specs.

-spec flatten([term()]) -> [term()].
flatten(Xs) -> lists:flatten(Xs).
