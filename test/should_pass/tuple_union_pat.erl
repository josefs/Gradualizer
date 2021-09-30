-module(tuple_union_pat).

-compile([export_all, nowarn_export_all]).

%% This is too vague for exhaustiveness checking.
%-spec f(tuple() | integer()) -> ok.
%f({1, 2}) ->
%    ok.

%% Using the guard, on the other hand, disables exhaustiveness checking completely.
-spec f({1,2} | integer()) -> ok.
f({1, 2}) -> ok;
f(I) when is_integer(I) -> ok.

-spec g({ok, binary()} | {error, term()}) -> integer().
g({error, key_not_found} = _Response) ->
    1;
g({error, _} = _Response) ->
    2;
g({ok, _} = _Response) ->
    3.
