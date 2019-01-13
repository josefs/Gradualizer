-module(record_index).

-export([f/0, g/0,
         undefined_record/0,
         undefined_record_infer/0,
         undefined_field/0,
         undefined_field_infer/0]).

-record(rec, { apa :: integer()}).

-spec f() -> boolean().
f() ->
    #rec.apa.

-spec g() -> 2.
g() ->
    #rec.apa.

-spec undefined_record() -> integer().
undefined_record() ->
    #undef.apa.

undefined_record_infer() ->
    #undef.apa.

-spec undefined_field() -> integer().
undefined_field() ->
    #rec.undef.

undefined_field_infer() ->
    #rec.undef.
