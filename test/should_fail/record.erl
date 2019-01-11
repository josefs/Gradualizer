-module(record).

-export([g/0,
         undefined_record/0,
         undefined_record_infer/0,
         undefined_field/0,
         undefined_field_infer/0]).

-record(rec, { apa :: integer()}).

-spec g() -> integer().
g() ->
    #rec{apa = 1}.

-spec undefined_record() -> term().
undefined_record() ->
    #undef{apa = 1}.

undefined_record_infer() ->
    #undef{apa = 1}.

-spec undefined_field() -> term().
undefined_field() ->
    #rec{undef = 1}.

undefined_field_infer() ->
    #rec{undef = 1}.
