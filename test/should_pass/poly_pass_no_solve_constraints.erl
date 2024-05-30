-module(poly_pass_no_solve_constraints).

%% The `-gradualizer([solve_constraints])' is intentionally missing.
-compile([export_all, nowarn_export_all]).

-spec id(A) -> A.
id(X) -> X.

-spec f(apple) -> banana.
f(X) -> id(X).

-spec g(boolean()) -> binary().
g(Bool) ->
    NewBool = id(Bool),
    NewBool.
