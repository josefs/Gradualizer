-module(unary_plus_fail).
-export([m/1, o/1, p/0]).

-spec m(+1) -> {}.
m(+2) ->
    {}.

-spec o(boolean()) -> boolean().
o(X) ->
    +X.

p() ->   
    +m(+1).
