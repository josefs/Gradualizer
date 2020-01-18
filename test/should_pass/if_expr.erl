-module(if_expr).

-export([foo/1]).

-spec foo(integer()) -> atom().
foo(X) ->
    A = if X < 0   -> neg;
           X =:= 0 -> zero;
           X > 0   -> pos
        end,
    A.
