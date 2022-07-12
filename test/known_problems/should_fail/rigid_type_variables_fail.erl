-module(rigid_type_variables_fail).

-compile([export_all, nowarn_export_all]).

-spec foo(A) -> A.
foo(X) -> {X, X}.
