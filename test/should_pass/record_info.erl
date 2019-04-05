-module(record_info).

-export([fields/0, size/0]).

-record(fruitz, {ananas, bananas :: atom(), cananas = 2}).

-spec fields() -> [ananas | bananas | cananas].
fields() -> record_info(fields, fruitz).

-spec size() -> 4.
size() ->
    N = record_info(size, fruitz),
    N.
