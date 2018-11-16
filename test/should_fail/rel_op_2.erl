-module(rel_op_2).

-spec fail(a | b, b | c) -> boolean() | tuple().
fail(X, Y) -> X == Y.
