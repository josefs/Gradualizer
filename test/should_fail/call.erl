-module(call).

-export([g/0, h/1, apply/1, remote/0]).

-spec f() -> {}.
f() ->
    {}.

-spec g() -> boolean().
g() ->
    f().

-spec h(boolean()) -> integer().
h(B) ->
    B().

-spec apply(fun((...) -> boolean())) -> integer().
apply(F) ->
    F().

-spec remote() -> integer().
remote() ->
    io_lib:format("", []).
