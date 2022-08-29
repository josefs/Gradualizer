-module(named_fun_fail).

-export([bar/0, baz/1]).

-spec foo(integer()) -> integer().
foo(N) -> N.

bar() -> foo(fun F(0) -> 0; F(X) -> F(X - 1) end).

-spec baz(integer()) -> boolean().
baz(I) ->
    O = I({}),
    O.
