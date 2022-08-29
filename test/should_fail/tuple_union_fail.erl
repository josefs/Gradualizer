-module(tuple_union_fail).

-export([f/0]).
-export([tuple_union/0]).

-spec f() -> {integer()} | {boolean()}.
f() ->
    {apa}.

-spec tuple_union() -> {undefined, binary()} | {integer(), undefined}.
tuple_union() ->
    {undefined, undefined}.
