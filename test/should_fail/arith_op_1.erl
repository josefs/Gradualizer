-module(arith_op_1).

-spec fail(_) -> tuple().
fail(X) -> X + X.
