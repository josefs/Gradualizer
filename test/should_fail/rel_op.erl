-module(rel_op).
-export([fail/1, fail/2]).

-spec fail(term()) -> tuple().
fail(X) -> X > X.

-spec fail(a | b, b | c) -> boolean() | tuple().
fail(X, Y) -> X == Y.
