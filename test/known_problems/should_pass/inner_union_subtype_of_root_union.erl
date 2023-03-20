-module(inner_union_subtype_of_root_union).

-export([tuple_case/0, map_case/1]).

% The problem is that {t, a|b} is not subtype of {t, a} | {t, b}.

% It has surfaced because
%     {'type', anno(), 'map_field_assoc' | 'map_field_exact', [abstract_type()]}
% is not a subtype of
%     {'type', anno(), 'map_field_assoc', [abstract_type()]}
%   | {'type', anno(), 'map_field_exact', [abstract_type()]}

-spec g() -> a | b.
g() -> a.

-spec tuple_case() -> {t, a} | {t, b}.
tuple_case() ->
    {t, g()}.

% The same thing holds for maps.
-spec map_case(#{a => b | c}) -> #{a => b} | #{a => c}.
map_case(M) -> M.