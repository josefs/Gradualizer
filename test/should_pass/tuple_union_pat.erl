-module(tuple_union_pat).

-compile([export_all, nowarn_export_all]).

-spec f(tuple() | integer()) -> ok.
f({1, 2}) ->
    ok.
