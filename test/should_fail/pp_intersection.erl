-module(pp_intersection).
-export([bar/1]).

%% Mostly to make sure pretty printing intersection types doesn't crash.

-spec cast(_) -> _.
cast(X) -> X.

-spec foo(integer()) -> float();
         (Float) -> integer() when Float :: float().
foo(N) when is_integer(N) -> cast(N * 1.0);
foo(X) when is_float(X) -> cast(round(X)).

-spec bar(boolean()) -> boolean().
bar(X) -> foo(X).

