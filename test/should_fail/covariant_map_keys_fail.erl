-module(covariant_map_keys_fail).

-compile([nowarn_unused_function]).
-export([not_good/1]).

-spec good(#{ good := A }) -> A.
good(#{ good := X }) -> X.

-spec not_good(#{good | bad := A}) -> A.
not_good(M) -> good(M). %% This call should fail

-spec kaboom() -> integer().
kaboom() -> not_good(#{ bad => 0 }).
