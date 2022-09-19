-module(covariant_map_keys_fail).

-export([not_good/1]).

-spec good(#{good := A}) -> A.
good(#{good := X}) -> X.

-spec not_good(#{good | bad := A}) -> A.
not_good(M) -> good(M). %% This call should fail
