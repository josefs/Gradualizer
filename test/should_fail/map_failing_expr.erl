-module(map_failing_expr).
-gradualizer([infer]).

-export([foo/0, bar/0]).

-spec foo() -> ok.
foo() ->
    #{ok => 42}.

-spec bar() -> ok.
bar() ->
    A = #{ok => 42},
    A.
