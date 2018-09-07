-module(tuple_union_fail).

-export([f/0]).

-spec f() -> {integer()} | {boolean()}.
f() ->
    {apa}.
