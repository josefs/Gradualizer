-module(tuple_union_pass).

-export([f/0,
         g/0]).

-spec f() -> {integer()} | {boolean()}.
f() ->
    {true}.

-type t() :: {float()} | {binary()}.

-spec g() -> t() | {integer()} | {boolean()}.
g() ->
    {true}.
