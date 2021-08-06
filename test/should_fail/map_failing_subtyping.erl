-module(map_failing_subtyping).

-export([m1/0, m2/0]).

-spec m1() -> #{bar => optional}.
m1() ->
    #{foo => bar}.

-spec m2() -> #{foo := atom(), bar := any()}.
m2() ->
    m().

-spec m() -> #{foo := atom(), bar => optional}.
m() ->
    #{foo => bar}.
