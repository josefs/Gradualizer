-module(lambda_not_fun).

-compile([export_all, nowarn_export_all]).

-spec foo(integer()) -> integer().
foo(N) -> N.

bar() -> foo(fun() -> 0 end).
