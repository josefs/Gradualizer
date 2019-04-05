-module(record_info_fail).

-export([fields/0, size/0]).

-record(fruitz, {ananas, bananas :: atom(), cananas = 2}).

-spec fields() -> [ananas | bananas].
fields() -> record_info(fields, fruitz).

-spec size() -> 3.
size() -> record_info(size, fruitz).
