-module(list_union_fail).

-compile(export_all).

-spec foo([a] | [b]) -> ok.
foo(_) -> ok.

%% Should fail since c has neither type a or b. Shouldn't crash.
bar() -> foo([c]).

%% Also for list comprehensions.
baz() -> foo([c || _ <- [1, 2]]).
