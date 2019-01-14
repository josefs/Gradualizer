-module(match).
-export([fail/0]).

-spec foo() -> integer().
foo() -> 0.

fail() -> X = [X = foo()].
