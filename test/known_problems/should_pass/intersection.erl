-module(intersection).
-export([inc/1]).

-spec inc(number()) -> number().
inc(N) -> N + 1.
