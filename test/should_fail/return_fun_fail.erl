-module(return_fun_fail).

-export([return_fun_union/0,
	 return_fun_remote/0
	]).

-spec return_fun_union() -> integer() | fun(() -> atom()).
return_fun_union() ->
    fun nil/0.

-spec return_fun_remote() -> fun((...) -> atom()).
return_fun_remote() ->
    fun erlang:atom_to_list/1.

-spec nil() -> [].
nil() -> [].
