-module(return_fun).

-export([return_fun_term/0,
         return_fun_fun/0,
         return_fun_union/0,
         return_fun_any_arity/0]).

-spec return_fun_term() -> term().
return_fun_term() -> fun nil/0.

-spec return_fun_fun() -> fun().
return_fun_fun() -> fun nil/0.

-spec return_fun_union() -> integer() | fun().
return_fun_union() -> fun nil/0.

-spec return_fun_any_arity() -> fun((...) -> list()).
return_fun_any_arity() -> fun nil/0.

-spec nil() -> [].
nil() -> [].
