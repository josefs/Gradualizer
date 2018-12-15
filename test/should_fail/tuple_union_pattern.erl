-module(tuple_union_pattern).

-export([tuple_union/1]).

-spec tuple_union({undefined, {}}
                | {{}, undefined}) -> {}.
tuple_union({undefined, undefined}) ->
    {};
tuple_union({{},{}}) ->
    {}.

