-module(unary_op).
-export([fail/1]).

-spec fail(number()) -> boolean().
fail(X) -> -X.
