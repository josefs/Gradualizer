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

%% By default inferring type from expressions with no spec is disabled.
%% So the type of `fun no_spec/0' is `any()' which is a subtype of
%% `integer()'.
-spec return_fun_no_spec() -> integer().
return_fun_no_spec() -> fun no_spec/0.

-spec nil() -> [].
nil() -> [].

-spec number(integer()) -> integer();
            (float()) -> float().
number(N) -> N.

no_spec() -> ok.
