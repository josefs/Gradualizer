-module(list_op_1).

-spec fail([a]) -> {ok, [a]}.
fail(Xs) -> Xs -- Xs.
