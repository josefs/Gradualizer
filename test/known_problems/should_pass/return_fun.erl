-module(return_fun).

-export([return_fun/0, return_fun2/0]).

-spec return_fun() -> term().
return_fun() -> fun nil/0.

-spec return_fun2() -> fun().
return_fun2() -> fun nil/0.

-spec nil() -> [].
nil() -> [].
