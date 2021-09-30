-module(alias_in_pattern).

-compile([export_all, nowarn_export_all]).

%% Too vague:
%-spec foo(tuple() | integer()) -> tuple().
-spec foo({_, _} | 42) -> tuple().
foo(Left = {_X, _Y})       -> Left;
foo(42 = N)                -> {N+N}.

%% Too vague:
%-spec bar(tuple() | integer()) -> tuple().
-spec bar({_,_,_} | 33) -> tuple().
bar({_,_,_} = Right) -> Right;
bar(N = 33)          -> {N+N}.

%% Misleading - there's no integer() clause at all:
%-spec baz(tuple() | integer()) -> tuple().
-spec baz({{}, 0}) -> tuple().
baz({Inside, _} = {{}, 0}) -> Inside.

-spec issue32({atom1, atom2} | atom4) -> {atom1, atom2} | atom3.
issue32(Stuff) ->
    case Stuff of
        {atom1, _} = X -> X;
        _ -> atom3
    end.

