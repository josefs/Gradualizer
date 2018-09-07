-module(tuple_union).

-export([f/0]).

-spec f() -> {integer()} | {boolean()}.
f() ->
    {true}.
