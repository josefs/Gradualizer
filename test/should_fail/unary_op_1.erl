-module(unary_op_1).

-spec fail(number()) -> boolean().
fail(X) -> -X.

