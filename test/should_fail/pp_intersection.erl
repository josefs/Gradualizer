-module(pp_intersection).

-compile([export_all, nowarn_export_all]).

%% Mostly to make sure pretty printing intersection types doesn't crash.

-spec cast(_) -> _.
cast(X) -> X.

-spec foo(integer()) -> float();
         (Float) -> integer() when Float :: float().
foo(N) when is_integer(N) -> cast(N * 1.0);
foo(X) when is_float(X) -> cast(round(X)).

-spec bar(boolean()) -> boolean().
bar(X) -> foo(X).

