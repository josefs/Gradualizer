-module(union_fun).

-export([
    union_fun_arg/2,
    union_fun_arg_var/2
]).

%% Prints the following error:
%% The variable on line 14 at column 5 is expected to have type fun((...) -> any())
%% but it has type fun((integer()) -> integer()) | fun((number()) -> atom())

-spec union_fun_arg(fun ((integer()) -> integer()) | fun ((number()) -> atom()), integer()) -> integer() | atom().
union_fun_arg(F, Int) ->
    F(Int).

-spec union_fun_arg_var(fun ((integer()) -> integer()) | fun ((number()) -> atom()), integer()) -> integer() | atom().
union_fun_arg_var(F, Int) ->
    X = F(Int),
    X.
