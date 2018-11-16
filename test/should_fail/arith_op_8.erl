-module(arith_op_8).

-spec fail(non_neg_integer(), neg_integer()) -> non_neg_integer().
fail(X, Y) -> X - Y.
