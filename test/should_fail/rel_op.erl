-module(rel_op).
-export([fail/2, foo/2, bar/2]).

-spec fail(a | b, b | c) -> boolean() | tuple().
fail(X, Y) -> X == Y.

-spec foo(atom(), atom()) -> integer().
foo(A, B) ->
    A = A < B,
    A.

-spec bar(atom(), float()) -> boolean().
bar(A, B) ->
    A = A < B,
    A.
