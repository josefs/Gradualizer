-module(list_op_2).

-spec fail(integer()) -> _ | integer().
fail(X) -> [1] ++ X.

