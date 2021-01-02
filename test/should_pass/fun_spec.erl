-module(fun_spec).

-compile([export_all, nowarn_export_all]).

-spec f() -> fun((any()) -> any()).
f() -> fun (X) -> X end.
