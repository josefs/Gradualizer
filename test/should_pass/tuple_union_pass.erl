-module(tuple_union_pass).

-export([f/0]).

-spec f() -> {integer()} | {boolean()}.
f() ->
    {true}.
