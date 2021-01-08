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

%% expected error message in the console:
%%     Example values which are not covered:
%%     #record_two_fields{
%%       a = #record_one_field{
%%         inner_rec = undefined
%%       },
%%       b = 0
%%     }
-spec two_fields(#record_two_fields{} | undefined) -> integer().
two_fields(#record_two_fields{a = #record_one_field{inner_rec = InnerRec = #inner{}}}) -> InnerRec#inner.field;
%% unhandled
%%two_fields(#record_two_fields{a = #record_one_field{inner_rec = undefined}}) -> 2;
two_fields(#record_two_fields{b = undefined}) -> 0;
%% unhandled
%%two_fields(#record_two_fields{b = B}) -> B;
two_fields(undefined) -> 0.



%% expected error message in the console:
%%     Example values which are not covered:
%%     #union_rec{
%%       foo = c
%%       bar = -1
%%     }
-record(union_rec, {
    foo :: a | b | c,
    bar :: float()       %% non-refinable field
}).
-spec union_rec(#union_rec{}) -> integer().
union_rec(#union_rec{foo = a}) -> 0;
union_rec(#union_rec{foo = b}) -> 1.
%% unhandled
%%union_rec(#union_rec{foo = c}) -> 2.

%% expected error message:
%%     Example values which are not covered:
%%     #empty_rec{}
-record(empty_rec, {}).
-spec empty_rec(#empty_rec{} | undefined) -> integer().
empty_rec(undefined) -> 1.
