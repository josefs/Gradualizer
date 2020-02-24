-module(record_refinement_fail).

-compile(export_all).

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

-spec refined_field2(#refined_field{}) -> #refined_field{f :: atom()}.
refined_field2(#refined_field{f = undefined}) -> #refined_field{f = 0};
refined_field2(R) -> R.

