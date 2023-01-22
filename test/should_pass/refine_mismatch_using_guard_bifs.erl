-module(refine_mismatch_using_guard_bifs).

-export([refine_by_guards_1/1,
         refine_by_guards_2/1]).

-spec refine_by_guards_1(atom() | pos_integer() | float() |
                         binary() | tuple() | [any()] |
                         map() | pid() | reference() | port()) -> ok | port().
refine_by_guards_1(X) when is_atom(X)      -> ok;
refine_by_guards_1(X) when is_number(X)    -> ok;
refine_by_guards_1(X) when is_bitstring(X) -> ok;
refine_by_guards_1(X) when is_tuple(X)     -> ok;
refine_by_guards_1(X) when is_list(X)      -> ok;
refine_by_guards_1(X) when is_map(X)       -> ok;
refine_by_guards_1(X) when is_pid(X)       -> ok;
refine_by_guards_1(X) when is_reference(X) -> ok;
refine_by_guards_1(X)                      -> X.

-spec refine_by_guards_2(neg_integer() | 2..4 | boolean() |
                         [a, ...] | port() | ok) -> ok.
refine_by_guards_2(X) when is_integer(X) -> ok;
refine_by_guards_2(X) when is_boolean(X) -> ok;
refine_by_guards_2(X) when is_list(X)    -> ok;
refine_by_guards_2(X) when is_port(X)    -> ok;
refine_by_guards_2(X)                    -> X.
