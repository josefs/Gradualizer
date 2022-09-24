-module(lists_map_nonempty_fail).

-export([f/1]).

-spec f([A]) -> [A, ...].
f(L) ->
    lists:map(fun (El) -> El end, L).
