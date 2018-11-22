-module(alias_in_pattern).
-export([foo/1, issue32/1]).

-spec foo(tuple() | integer()) -> tuple().
foo(Left = {_X, _Y})       -> Left;
foo({_,_,_} = Right)       -> Right;
foo({Inside, _} = {{}, 0}) -> Inside;
foo(42 = N)                -> {N+N}.

-spec issue32({atom1, atom2} | atom4) -> {atom1, atom2} | atom3.
issue32(Stuff) ->
    case Stuff of
        {atom1, _} = X -> X;
        _ -> atom3
    end.

