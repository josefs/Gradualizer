-module(negate_none).

-compile([export_all, nowarn_export_all]).

-spec crash() -> none().
crash() -> error(crash).

foo() -> -crash().

%-spec bar(none()) -> _.
%bar(X) -> -X.  %% <--- error "clause cannot be reached"

