-module(fun_capture).

-spec f(integer()) -> fun(([atom()]) -> [atom()]).
f(X) ->
    (fun (X) ->
	     X ++ []
     end).
