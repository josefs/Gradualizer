-module(list).

-compile([export_all, nowarn_export_all]).

-spec f([integer()]) -> integer().

f([]) ->
    0;
f([A|As]) ->
    A + f(As).

string_tail() ->
    [ 1 | atom_to_list(foo)].

-spec list_union_tail([atom()] | [integer()]) -> any().
list_union_tail(L) ->
    [ 1 | L ].

list_any_tail() ->
    [ 1 | return_list() ].

-spec return_list() -> nonempty_list().
return_list() ->
    [2, 3].
