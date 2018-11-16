-module(logic_op_1).

-spec fail(boolean()) -> tuple().
fail(X) -> X and X.
