-module(arith_op).
-export([plusintfloat/2, plusfloatint/2]).

-spec plusintfloat(integer(), float()) -> float().
plusintfloat(X, Y) -> X + Y.

-spec plusfloatint(float(), integer()) -> float().
plusfloatint(X, Y) -> X + Y.
