-module(record).

-export([g/0]).

-record(rec, { apa :: integer()}).

-spec g() -> integer().
g() ->
    #rec{apa = 1}.
