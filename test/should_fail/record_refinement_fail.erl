-module(record_refinement_fail).

-compile(export_all).

-record(one_field, {a :: integer() | undefined}).

-spec one_field(#one_field{}, integer()) -> integer().
one_field(#one_field{a = I}, _) -> I;
one_field(_, I) -> I.

-spec one_field2(#one_field{}, integer()) -> integer().
one_field2(R, _) -> R#one_field.a;
one_field2(_, I) -> I.
