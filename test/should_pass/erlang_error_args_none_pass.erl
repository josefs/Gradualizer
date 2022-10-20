-module(erlang_error_args_none_pass).

-export([f/0,
         g/0]).

-spec f() -> atom().
f() ->
    erlang:error(42, none),
    ok.

-spec g() -> atom().
g() ->
    erlang:error(42, none, []),
    ok.
