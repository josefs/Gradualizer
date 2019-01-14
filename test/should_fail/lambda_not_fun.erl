-module(lambda_not_fun).

-export([bar/0]).

-spec foo(integer()) -> integer().
foo(N) -> N.

bar() -> foo(fun() -> 0 end).
