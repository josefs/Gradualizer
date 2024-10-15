-module(exhaustive_expr_fail).

-export([f/1]).

-spec f(a | b) -> ok.
f(X) ->
    case X of
        a -> anything
    end,
    ok.
