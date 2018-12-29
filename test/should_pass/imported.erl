-module(imported).

-export([foo/1]).
-import(any, [any/1]).

-spec foo(integer()) -> atom().
foo(X) -> any(X).
