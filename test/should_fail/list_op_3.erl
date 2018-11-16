-module(list_op_3).

-spec fail([a, ...], [a]) -> [a, ...].
fail(Xs, Ys) -> Xs -- Ys.

