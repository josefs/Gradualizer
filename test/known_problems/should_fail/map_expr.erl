-module(map_expr).

-export([foo/0]).

-spec foo() -> ok.
foo() ->
    A = #{ok => 42},
    A.
