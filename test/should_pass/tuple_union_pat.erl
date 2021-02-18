-module(tuple_union_pat).

-compile([export_all, nowarn_export_all]).

-spec f(tuple() | integer()) -> ok.
f({1, 2}) ->
    ok.

-spec g({ok, binary()} | {error, term()}) -> integer().
g({error, key_not_found} = _Response) ->
    1;
g({error, _} = _Response) ->
    2;
g({ok, _} = _Response) ->
    3.
