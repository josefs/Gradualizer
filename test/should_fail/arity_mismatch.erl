-module(arity_mismatch).

-compile([export_all, nowarn_export_all]).

-spec foo(alice) -> bob.
foo(alice) -> bob.

-spec bar(fun((alice, bob) -> bob)) -> bob.
bar(F) -> F(alice).

-spec bar() -> bob.
bar() -> (fun foo/1)().
