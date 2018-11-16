-module(logic_op_2).

-spec fail(boolean()) -> tuple().
fail(X) -> X andalso {}.
