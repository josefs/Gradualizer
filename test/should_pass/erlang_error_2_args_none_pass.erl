-module(erlang_error_2_args_none_pass).

-export([f/0]).

-spec f() -> atom().
f() ->
    erlang:error(42, none),
    ok.
