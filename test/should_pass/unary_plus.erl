-module(unary_plus).

-compile([export_all, nowarn_export_all]).

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
