-module(unary_plus_fail).
-export([m/1, o/0, p/0]).

-spec m(+1) -> {}.
m(+2) ->
    {}.

-spec o() -> boolean().
o() ->
    +true.

p() ->   
    +m(+1).
