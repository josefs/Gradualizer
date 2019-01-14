-module(arity_mismatch).

-export([bar/0, bar/1]).

-spec foo(alice) -> bob.
foo(alice) -> bob.

-spec bar(fun((alice, bob) -> bob)) -> bob.
bar(F) -> F(alice).

-spec bar() -> bob.
bar() -> (fun foo/1)().
