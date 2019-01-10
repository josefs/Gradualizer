-module(named_fun).

-compile([export_all, nowarn_export_all]).

-spec foo(integer()) -> integer().
foo(N) -> N.

bar() -> foo(fun F(0) -> 0; F(X) -> F(X - 1) end).
