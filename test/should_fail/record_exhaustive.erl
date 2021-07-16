-module(record_exhaustive).

-compile([export_all, nowarn_export_all]).

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
%%     #record_one_field{
%%       inner_rec = undefined
%%     }
-spec one_field(#record_one_field{} | undefined) -> integer().
one_field(#record_one_field{inner_rec = InnerRec = #inner{}}) -> InnerRec#inner.field;
%% unhandled
%%one_field(#record_one_field{field = undefined}) -> -1;
one_field(undefined) -> 0.


-spec two_fields_b(#record_two_fields{} | undefined) -> integer().
two_fields_b(#record_two_fields{a = #record_one_field{inner_rec = InnerRec = #inner{}}}) -> InnerRec#inner.field;
two_fields_b(#record_two_fields{a = #record_one_field{inner_rec = undefined}}) -> 2;
% two_fields_b(#record_two_fields{b = undefined}) -> 0;
%% unhandled
two_fields_b(#record_two_fields{b = B}) -> B;
two_fields_b(undefined) -> 0.


%% expected error message:
%%     Example values which are not covered:
%%     #empty_rec{}
-record(empty_rec, {}).
-spec empty_rec(#empty_rec{} | undefined) -> integer().
%% unhandled
% empty_rec(#empty_rec{}) -> 0;
empty_rec(undefined) -> 1.
