-module(record).

-export([g/0, h/0]).

-record(rec, { apa :: integer()}).

-spec g() -> integer().
g() ->
    #rec{apa = 1}.

-spec h() -> integer().
h() ->
    Rec = #rec{},
    Rec#rec.apa.
