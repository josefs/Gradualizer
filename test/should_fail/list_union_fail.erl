-module(list_union_fail).
-export([bar/0, baz/0]).

-spec foo([a] | [b]) -> ok.
foo(_) -> ok.

%% Should fail since c has neither type a or b. Shouldn't crash.
bar() -> foo([c]).

%% Also for list comprehensions.
baz() -> foo([c || _ <- [1, 2]]).
