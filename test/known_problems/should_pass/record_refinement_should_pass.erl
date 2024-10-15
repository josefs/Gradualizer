-module(record_refinement_should_pass).

-export([refined_field/1,
         refined_field_safe/1,
         %refined_field_unsafe/1,
         not_all_fields_refined/0]).

-record(refined_field, {f :: integer() | undefined}).

-spec refined_field(#refined_field{}) -> #refined_field{f :: integer()}.
refined_field(#refined_field{f = undefined}) -> #refined_field{f = 0};
refined_field(R) -> R.

-spec refined_field_safe(#refined_field{f :: integer()}) -> #refined_field{f :: integer()}.
refined_field_safe(#refined_field{f = I}) -> #refined_field{f = I + 1}.

%% This doesn't fail, but calls refined_field_safe/1 which does,
%% so we can't have it in tests/should_pass.
-spec refined_field_unsafe(#refined_field{}) -> #refined_field{}.
refined_field_unsafe(R = #refined_field{f = undefined}) -> R;
refined_field_unsafe(R) -> refined_field_safe(R).

-record(not_all_fields_refined_r, {a, b}).
-type not_all_fields_refined_t() :: #not_all_fields_refined_r{b :: b | c}.

-spec not_all_fields_refined() -> not_all_fields_refined_t().
not_all_fields_refined() -> #not_all_fields_refined_r{b = c}.
