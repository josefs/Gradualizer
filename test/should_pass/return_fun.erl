-module(return_fun).

-compile([export_all, nowarn_export_all]).

-spec return_fun_term() -> term().
return_fun_term() -> fun nil/0.

-spec return_fun_fun() -> fun().
return_fun_fun() -> fun nil/0.

-spec return_fun_union() -> integer() | fun().
return_fun_union() -> fun nil/0.

-spec return_fun_any_arity() -> fun((...) -> list()).
return_fun_any_arity() -> fun nil/0.

-spec return_fun_intersection() -> fun((any()) -> integer()).
return_fun_intersection() -> fun number/1.

-spec return_fun_union_intersection()
                                   -> fun((atom()) -> atom()) |
                                      fun((1..3) -> integer()).
return_fun_union_intersection() -> fun number/1.

-spec nil() -> [].
nil() -> [].

-spec number(integer()) -> integer();
            (float()) -> float().
number(N) -> N.
