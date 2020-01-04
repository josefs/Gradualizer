-module(guard_fail).

-compile(export_all).

-spec wrong_guard(integer() | atom()) -> atom() | not_atom.
wrong_guard(A) when is_integer(A) -> A;
wrong_guard(_) -> not_atom.

-record(r1, {
    f
}).

-record(r2, {
    f
}).

-spec wrong_record(#r1{} | #r2{}) -> #r2{} | not_r2.
wrong_record(R) when is_record(R, r1) -> R;
wrong_record(_) -> not_r2.

-spec wrong_union(integer() | float() | atom()) -> integer() | float() | not_number.
wrong_union(I) when is_integer(I) -> I;
wrong_union(F) when is_atom(F) -> F;
wrong_union(_) -> not_number.

-spec wrong_union2(integer() | float() | atom()) -> integer() | float() | not_number.
wrong_union2(N) when is_integer(N) orelse is_atom(N) -> N;
wrong_union2(_) -> not_number.

-spec wrong_andalso(integer() | atom()) -> atom() | not_integer.
wrong_andalso(N) when is_integer(N) andalso is_atom(N) -> N;
wrong_andalso(_) -> not_integer.
