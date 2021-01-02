-module(float).

-compile([export_all, nowarn_export_all]).

-spec f() -> float().
f() ->
    2.0.

-spec g() -> float() | banana.
g() ->
    3.14.

issue68_a(1.0) -> banana;
issue68_a(_) -> apple.

-spec issue68_b(_) -> apple | banana.
issue68_b(1.0) -> banana;
issue68_b(_) -> apple.
