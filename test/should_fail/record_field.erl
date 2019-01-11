-module(record_field).

-export([f/1, g/1,
         undefined_record/1,
         undefined_record_infer/1,
         undefined_field/1,
         undefined_field_infer/1]).

-record(rec, { apa :: integer()}).

-spec f(#rec{}) -> boolean().
f(A) ->
    A#rec.apa.

-spec g(#rec{}) -> pos_integer().
g(A) ->
    A#rec.apa.

-spec undefined_record(any()) -> integer().
undefined_record(A) ->
    A#undef.apa.

undefined_record_infer(A) ->
    A#undef.apa.

-spec undefined_field(#{rec}) -> any().
undefined_field(A) ->
    A#rec.undef.

undefined_field_infer(A) ->
    A#rec.undef.
