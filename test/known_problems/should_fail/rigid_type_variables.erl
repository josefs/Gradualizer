-module(rigid_type_variables).

-compile([export_all, nowarn_export_all]).

-spec foo(A) -> A.
foo(X) -> {X, X}.
