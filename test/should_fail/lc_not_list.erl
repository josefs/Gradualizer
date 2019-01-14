-module(lc_not_list).

-export([bar/0, baz/0]).

-spec foo(integer()) -> integer().
foo(N) -> N.

bar() -> foo([ X || X <- [1, 2]]).
baz() -> foo(<< <<X>> || X <- [1, 2] >>).
