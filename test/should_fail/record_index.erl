-module(record_index).

-export([f/0]).

-record(rec, { apa :: integer()}).

-spec f() -> boolean().
f() ->
    #rec.apa.
