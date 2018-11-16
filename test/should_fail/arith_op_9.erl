-module(arith_op_9).

-spec fail(neg_integer(), non_neg_integer()) -> neg_integer().
fail(X, Y) -> X - Y.
