-module(lc_not_list).

-compile([export_all, nowarn_export_all]).

-spec foo(integer()) -> integer().
foo(N) -> N.

bar() -> foo([ X || X <- [1, 2]]).
baz() -> foo(<< <<X>> || X <- [1, 2] >>).
