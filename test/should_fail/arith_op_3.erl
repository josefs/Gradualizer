-module(arith_op_3).

-spec fail(boolean()) -> any() | boolean().
fail(X) -> X div 2.
