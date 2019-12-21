-module(record_refinement_fail).

-compile(export_all).

-record(one_field, {a :: integer() | undefined}).

-spec one_field(#one_field{}, integer()) -> integer().
one_field(#one_field{a = I}, _) -> I;
one_field(_, I) -> I.
