-module(rel_op_1).

-spec fail(term()) -> tuple().
fail(X) -> X > X.
