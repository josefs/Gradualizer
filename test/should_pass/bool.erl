-module(bool).

-compile([export_all, nowarn_export_all]).

-spec b(boolean(), boolean()) -> boolean().
b(B1, B2) ->
    B1 andalso B2.

%% variable bindings should propagate from first to second arg of orelse
-spec b2() -> boolean().
b2() ->
    begin
        A = f(),
        is_integer(A)
    end
        orelse
        is_float(A).

-spec f() -> number().
f() -> 1.
