-module(infinite_loop3).

-compile([export_all, nowarn_export_all]).

-record(rec, {rec :: any()}).
-type rec(A) :: A | #rec{rec :: A | rec(A)}.

-spec unwrap(any()) -> rec(integer()).
unwrap(_) -> 1.
