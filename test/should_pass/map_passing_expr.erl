-module(map_passing_expr).

-export([foo/0]).

-spec foo() -> #{atom() => integer()}.
foo() ->
    A = #{ok => 42},
    A.
