-module(arith_op_5).

-spec fail(1..10) -> 1..10 | atom.
fail(X) -> X div X.
