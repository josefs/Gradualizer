-module(fun_spec).

-spec f() -> fun((any()) -> any()).
f() -> fun (X) -> X end.
