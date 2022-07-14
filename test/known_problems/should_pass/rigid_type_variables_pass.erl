-module(rigid_type_variables_pass).

-compile([export_all, nowarn_export_all]).

-spec id(A) -> A.
id(X) -> X.

%% Currently crashes in int_type_to_range/1 on {var, _, 'A'}.
foo() -> -id(42).
