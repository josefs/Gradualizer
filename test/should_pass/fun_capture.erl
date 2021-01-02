-module(fun_capture).

-compile([nowarn_unused_vars, nowarn_shadow_vars]).
-compile([export_all, nowarn_export_all]).

-spec f(integer()) -> fun(([atom()]) -> [atom()]).
f(X) ->
    (fun (X) ->
	     X ++ []
     end).
