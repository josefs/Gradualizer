-module(record_refinement).

-export([one_field/2,
         one_field2/2,
         two_field/2,
         multiple/1,
         underscore/1,
         type_var/1,
         with_spec/1,
         two_level1/1,
         two_level2/1]).

-record(one_field, {a :: integer() | undefined}).

-spec one_field(#one_field{}, integer()) -> integer().
one_field(#one_field{a = undefined}, I) -> I;
one_field(#one_field{a = I}, _) -> I.

-spec one_field2(#one_field{}, integer()) -> integer().
one_field2(#one_field{a = undefined}, I) -> I;
one_field2(R, _) -> R#one_field.a.

-record(two_field, {a :: atom(), b :: integer() | undefined}).

-spec two_field(#two_field{}, integer()) -> integer().
two_field(#two_field{b = undefined}, I) -> I;
two_field(#two_field{b = I}, _) -> I.

-record(multiple, {a :: integer() | undefined, b :: integer() | undefined}).

-spec multiple(#multiple{}) -> integer().
multiple(#multiple{a = undefined, b = undefined}) -> 0;
multiple(#multiple{a = undefined, b = B}) -> B;
multiple(#multiple{a = A, b = undefined}) -> A;
multiple(#multiple{a = A, b = B}) -> A + B.

-record(underscore, {a :: integer() | undefined, b :: integer() | undefined, c :: integer() | undefined}).

-spec underscore(#underscore{}) -> integer().
underscore(#underscore{_ = undefined}) -> 0;
underscore(#underscore{a = A, _ = undefined}) -> A;
underscore(#underscore{b = B, _ = undefined}) -> B;
underscore(#underscore{c = C, _ = undefined}) -> C;
underscore(#underscore{a = A, b = B, _ = undefined}) -> A + B;
underscore(#underscore{a = A, c = C, _ = undefined}) -> A + C;
underscore(#underscore{b = B, c = C, _ = undefined}) -> B + C;
underscore(#underscore{a = A, b = B, c = C}) -> A + B + C.

-record(type_var, {f :: integer()}).
-spec type_var([#type_var{}]) -> [integer()].
type_var(Rs) -> lists:map(fun (R) -> R#type_var.f end, Rs).

-record(any, {f :: integer()}).
without_spec(R) -> with_spec(R#any.f).

-spec with_spec(integer()) -> integer().
with_spec(I) -> I + 1.

-record(two_level2, {value :: undefined | integer()}).
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
