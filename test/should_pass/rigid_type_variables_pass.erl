-module(rigid_type_variables_pass).

-compile([export_all, nowarn_export_all]).

-spec id(A) -> A.
id(X) -> X.

%% Used to cause a crash in int_type_to_range/1 on {var, _, 'A'}.
foo() -> -id(42).
