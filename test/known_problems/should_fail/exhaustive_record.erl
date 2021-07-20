-module(exhaustive_record).

-compile([export_all, nowarn_export_all]).


%% This file copies the structure from test/should_fail/record_exhaustive.erl

-record(inner, {
    field :: integer()
}).

-record(record_one_field, {
    inner_rec :: #inner{} | undefined
}).

-record(record_two_fields, {
    a :: #record_one_field{},
    b :: integer() | undefined
}).

%% expected error message in the console:
%%     Example values which are not covered:
%%     #record_two_fields{
%%       a = #record_one_field{
%%         inner_rec = undefined
%%       },
%%     }
-spec two_fields_inner(#record_two_fields{} | undefined) -> integer().
two_fields_inner(#record_two_fields{a = #record_one_field{inner_rec = InnerRec = #inner{}}}) -> InnerRec#inner.field;
%% unhandled
% two_fields_inner(#record_two_fields{a = #record_one_field{inner_rec = undefined}}) -> 2;
two_fields_inner(#record_two_fields{b = undefined}) -> 0;
two_fields_inner(#record_two_fields{b = B}) -> B;
two_fields_inner(undefined) -> 0.
