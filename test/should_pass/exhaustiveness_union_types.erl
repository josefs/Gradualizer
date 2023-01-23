-module(exhaustiveness_union_types).

-export([g/0]).

-spec f() -> ok | {error, foo} | {error, bar}.
f() -> ok.

-spec g() -> integer().
g() ->
    case f() of
        ok -> 42;
        {error, _Sth} -> 43
    end.