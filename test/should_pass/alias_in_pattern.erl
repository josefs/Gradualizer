-module(alias_in_pattern).

-compile([export_all, nowarn_export_all]).

-spec foo(tuple() | integer()) -> tuple().
foo(Left = {_X, _Y})       -> Left;
foo(42 = N)                -> {N+N}.

-spec bar(tuple() | integer()) -> tuple().
bar({_,_,_} = Right) -> Right;
bar(N = 33)          -> {N+N}.

-spec baz(tuple() | integer()) -> tuple().
baz({Inside, _} = {{}, 0}) -> Inside.

-spec issue32({atom1, atom2} | atom4) -> {atom1, atom2} | atom3.
issue32(Stuff) ->
    case Stuff of
        {atom1, _} = X -> X;
        _ -> atom3
    end.

