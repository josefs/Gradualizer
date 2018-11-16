-module(arith_op_6).

-spec fail(5, 2 | 4) -> 7 | 9.
fail(X, Y) -> X + Y.
