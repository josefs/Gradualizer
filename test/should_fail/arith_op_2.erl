-module(arith_op_2).

-spec fail(_) -> boolean().
fail(X) -> X div X.
