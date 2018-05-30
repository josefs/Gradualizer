-module(tuple_union2).

-compile(export_all).

-spec tuple_union({undefined, {}}
                | {{}, undefined}) -> {}.
tuple_union({undefined, undefined}) ->
    {};
tuple_union({{},{}}) ->
    {}.

