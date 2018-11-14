-module(unary_plus).

-spec m(+1) -> {}.
m(+1) ->
    {}.

n(+1) ->
    {}.

-spec o() -> integer().
o() ->
    +5.


p() ->
    +5.
