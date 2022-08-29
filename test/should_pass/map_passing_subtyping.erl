-module(map_passing_subtyping).

-export([m1/0, m2/0]).

-spec m1() -> #{foo := atom(), bar => optional}.
m1() ->
    #{foo => bar}.

-spec m2() -> #{foo => atom()}.
m2() ->
    #{foo => bar}.
