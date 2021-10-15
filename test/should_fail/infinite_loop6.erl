-module(infinite_loop6).

-compile([export_all, nowarn_export_all]).

-type rec1(A) :: A | rec1(A).

-spec unwrap(rec1(integer())) -> atom().
%% `ok' is not an `integer()' - this should fail.
unwrap(ok) -> ok.
