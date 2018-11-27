-module(covariant_map_keys).

-compile([export_all, nowarn_export_all]).

-spec good(#{ good := A }) -> A.
good(#{ good := X }) -> X.

-spec not_good(#{good | bad := A}) -> A.
not_good(M) -> good(M). %% This call should fail

-spec kaboom() -> integer().
kaboom() -> not_good(#{ bad => 0 }).

