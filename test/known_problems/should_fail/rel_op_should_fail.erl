-module(rel_op_should_fail).
-export([fail/1]).

-spec fail(term()) -> tuple().
fail(X) -> X > X.
