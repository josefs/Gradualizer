-module(float).

-compile([export_all]).

-spec f() -> float().
f() ->
    2.0.

-spec g() -> float() | banana.
g() ->
    3.14.
