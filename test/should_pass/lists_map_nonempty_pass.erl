-module(lists_map_nonempty_pass).

-export([f/1,
         g/1]).

-spec f([A, ...]) -> [A, ...].
f(L) ->
    lists:map(fun (El) -> El end, L).

%% [A, ...] is a subtype of [A], so this is fine, too,
%% even if lists:map/2 :: ((A) -> B, [A, ...]) -> [B, ...].
-spec g([A, ...]) -> [A].
g(L) ->
    lists:map(fun (El) -> El end, L).
