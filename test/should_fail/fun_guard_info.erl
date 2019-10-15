-module(fun_guard_info).
-compile([export_all]).

-spec fun_wrong_arity(any()) -> boolean().
fun_wrong_arity(Fun) when is_function(Fun, 2) ->
    Fun(0);
fun_wrong_arity(_Fun) -> true.

