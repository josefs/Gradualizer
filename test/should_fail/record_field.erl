-module(record_field).

-export([f/1, g/1]).

-record(rec, { apa :: integer()}).

-spec f(#rec{}) -> boolean().
f(A) ->
    A#rec.apa.

-spec g(#rec{}) -> pos_integer().
g(A) ->
    A#rec.apa.
