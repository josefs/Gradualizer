-module(inner_union_subtype_of_root_union_pass).

-export([tuple_case1/0,
         tuple_case2/0]).

% The problem is that {t, a|b} is not subtype of {t, a} | {t, b}.

% It has surfaced because
%     {'type', anno(), 'map_field_assoc' | 'map_field_exact', [abstract_type()]}
% is not a subtype of
%     {'type', anno(), 'map_field_assoc', [abstract_type()]}
%   | {'type', anno(), 'map_field_exact', [abstract_type()]}

% See also test/known_problems/should_pass/inner_union_subtype_of_root_union.erl

-spec g() -> a | b.
g() -> a.

-spec tuple_case1() -> {t, a} | {t, b}.
tuple_case1() ->
    R = {t, g()},
    R.

-spec tuple_case2() -> {t, a} | {t, b}.
tuple_case2() ->
    {t, g()}.
