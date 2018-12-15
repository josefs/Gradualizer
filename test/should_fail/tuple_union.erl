-module(tuple_union).

-export([tuple_union/0]).

-spec tuple_union() -> {undefined, binary()} | {integer(), undefined}.
tuple_union() ->
    {undefined, undefined}.
