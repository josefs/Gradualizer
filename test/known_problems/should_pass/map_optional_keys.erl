-module(map_optional_keys).

-export([m/0]).

-spec m() -> #{foo := atom(), bar => optional}.
m() ->
        #{foo => bar}.
