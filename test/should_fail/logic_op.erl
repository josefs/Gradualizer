-module(logic_op).
-export([failand/1, failandalso/1]).

-spec failand(boolean()) -> tuple().
failand(X) -> X and X.

-spec failandalso(boolean()) -> tuple().
failandalso(X) -> X andalso {}.
