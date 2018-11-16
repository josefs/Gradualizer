-module(arith_op_7).

-spec fail(pos_integer(), neg_integer()) -> pos_integer().
fail(X, Y) -> X - Y.
