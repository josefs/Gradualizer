-module(guard_should_fail).

-compile([export_all, nowarn_export_all]).

-spec wrong_arity(fun((any()) -> any()) | other) -> fun((any()) -> any()) | not_function.
wrong_arity(F) when is_function(F, 2) -> F;
wrong_arity(_) -> not_function.
