-module(record_field).

-export([f/1]).

-record(rec, { apa :: integer()}).

-spec f(#rec{}) -> boolean().
f(A) ->
    A#rec.apa.
