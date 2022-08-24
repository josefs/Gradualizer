-module(rigid_type_variables_pass).

-compile([export_all, nowarn_export_all]).

-spec id(A) -> A.
id(X) -> X.

%% Caused a crash in int_type_to_range/1 on {var, _, 'A'}.
%% It's now fixed by assuming that a type var means any integer whatsoever.
foo() -> -id(42).
