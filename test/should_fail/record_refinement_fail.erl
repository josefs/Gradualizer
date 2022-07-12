-module(record_refinement_fail).

-compile([export_all, nowarn_export_all]).

-record(one_field, {a :: integer() | undefined}).

-spec one_field(#one_field{}, integer()) -> integer().
one_field(#one_field{a = I}, _) -> I;
one_field(_, I) -> I.

-spec one_field2(#one_field{}, integer()) -> integer().
one_field2(R, _) -> R#one_field.a;
one_field2(_, I) -> I.

-record(refined_field, {f :: integer() | undefined}).
-spec refined_field(#refined_field{}) -> #refined_field{f :: integer()}.
refined_field(R) -> R.

%% The refinement in the result type is not handled by Dialyzer - it will error out.
%% Comment it out if trying to run `make dialyze-tests'.
-spec refined_field2(#refined_field{}) -> #refined_field{f :: atom()}.
refined_field2(#refined_field{f = undefined}) -> #refined_field{f = 0};
refined_field2(R) -> R.

-record(two_level2, {value :: undefined | binary()}).
-record(two_level1, {two_level2 :: undefined | #two_level2{}}).

-spec two_level1(#two_level1{}) -> integer().
two_level1(#two_level1{two_level2 = undefined}) -> 
    0;
two_level1(#two_level1{two_level2 = #two_level2{value = undefined}}) -> 
    0;
two_level1(#two_level1{two_level2 = #two_level2{value = Value}}) ->
    Value.

-spec two_level2(#two_level1{}) -> integer().
two_level2(#two_level1{two_level2 = undefined}) -> 
    0;
two_level2(#two_level1{two_level2 = #two_level2{value = undefined}}) -> 
    0;
two_level2(R1) ->
    R1#two_level1.two_level2#two_level2.value.
