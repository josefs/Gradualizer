-module(logic_op).
-export([failand/1, failand2/1,
         failandleft/2, failandleft2/2,
         failandright/2, failandright2/2,
         failandalso/1, failandalso2/1,
         failnot/1]).

-spec failand(boolean()) -> tuple().
failand(X) -> X and X.

-spec failand2(boolean()) -> tuple().
failand2(X) ->
    O = X and X,
    O.

-spec failandleft(boolean(), integer()) -> boolean().
failandleft(B, N) -> N and B.

-spec failandleft2(boolean(), integer()) -> boolean().
failandleft2(B, N) ->
    O = N and B,
    O.

-spec failandright(boolean(), integer()) -> boolean().
failandright(B, N) -> B and N.

-spec failandright2(boolean(), integer()) -> boolean().
failandright2(B, N) ->
    O = B and N,
    O.

-spec failandalso(boolean()) -> tuple().
failandalso(X) -> X andalso {}.

-spec failandalso2(boolean()) -> tuple().
failandalso2(X) ->
    O = X andalso {},
    O.

-spec failnot(integer()) -> boolean().
failnot(N) ->
    O = not(N),
    O.
