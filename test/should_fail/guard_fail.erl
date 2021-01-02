-module(guard_fail).

-compile([export_all, nowarn_export_all]).

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

-spec wrong_orelse(integer() | atom(), integer() | atom()) -> integer().
wrong_orelse(X, Y) when is_integer(X) orelse is_integer(Y) -> X + Y;
wrong_orelse(_, _) -> 42.

%% We currently don't infer X :: nonempty_list() from hd(X)
-spec other_guard_fun([true, ...] | boo) -> boolean().
other_guard_fun(X) when is_list(X); hd(X) -> length(X) > 3;
other_guard_fun(_) -> false.

-spec compare_still_float(number()) -> list().
compare_still_float(N) when 1 =< N, 10 >= N ->
    integer_to_list(N); % error: N can still be a float
compare_still_float(_) ->
    "X".

-spec equal_still_float(number()) -> list().
equal_still_float(N) when 0 == N ->
    integer_to_list(N); % error: N can still be float, since 0.0 == 0
equal_still_float(_) ->
    "X".
