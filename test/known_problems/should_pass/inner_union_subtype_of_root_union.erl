-module(inner_union_subtype_of_root_union).

-export([map_case/1]).

% The problem is that {t, a|b} is not subtype of {t, a} | {t, b}.

% It has surfaced because
%     {'type', anno(), 'map_field_assoc' | 'map_field_exact', [abstract_type()]}
% is not a subtype of
%     {'type', anno(), 'map_field_assoc', [abstract_type()]}
%   | {'type', anno(), 'map_field_exact', [abstract_type()]}

% See also test/should_pass/inner_union_subtype_of_root_union_pass.erl

-spec g() -> a | b.
g() -> a.

% The same thing holds for maps.
-spec map_case(#{a => b | c}) -> #{a => b} | #{a => c}.
map_case(M) -> M.
