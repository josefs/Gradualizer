-module(arith_op_4).

-spec fail(integer()) -> neg_integer().
fail(X) -> X / X.
