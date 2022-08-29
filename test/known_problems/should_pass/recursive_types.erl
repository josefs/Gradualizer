-module(recursive_types).

-compile([export_all, nowarn_export_all]).

-type rec(A) :: A | {rec, rec(A)}.

-spec unwrap(rec(rec(atom()))) -> rec(atom()).
unwrap({rec, Elem}) -> Elem.
