-module(type_refinement).

-export([basic_type_refinement/1,
         int_literals_1/1, int_literals_2/1, int_literals_3/1,
         disjoint_stuff_1/1, disjoint_stuff_2/1,
         disjoint_stuff_3/1, disjoint_stuff_4/1,
         var_pat/2, nil_elimination/1,
         tuple_union/1, beside_match_all/2, beside_singleton/2]).

%% Test that Value is not considered to be string() | false.
-spec basic_type_refinement(string()) -> string().
basic_type_refinement(Key) ->
    case getenv(Key) of
        false -> "banana";
        Value -> Value
    end.


%% A simplified version of os:getenv/1.
-spec getenv(Key :: string()) -> Value :: string() | false.
getenv("SHELL") -> "/bin/sh";
getenv("USER")  -> "joe";
getenv(_)       -> false.

-spec int_literals_1(integer()) -> pos_integer() | neg_integer().
int_literals_1(0) -> 42;
int_literals_1(N) -> N.

-spec int_literals_2(0..10) -> 0..4 | 6..9.
int_literals_2(10) -> 0;
int_literals_2(5) -> 2;
int_literals_2(N) -> N.

-spec int_literals_3(non_neg_integer()) -> pos_integer().
int_literals_3(0) -> 42;
int_literals_3(N) -> N.

-spec disjoint_stuff_1(non_neg_integer() | -3) -> non_neg_integer().
disjoint_stuff_1(-3) -> 42; %% this eliminates -3 but doesn't affect neg_integer()
disjoint_stuff_1(X) -> X.

-spec disjoint_stuff_2(pos_integer() | -3) -> pos_integer().
disjoint_stuff_2(-3) -> 42; %% this eliminates 42 but doesn't affect neg_integer()
disjoint_stuff_2(X) -> X.

-spec disjoint_stuff_3(neg_integer() | 42) -> neg_integer().
disjoint_stuff_3(42) -> -3; %% this eliminates 42 but doesn't affect neg_integer()
disjoint_stuff_3(X) -> X.

-spec disjoint_stuff_4(1 | 3 | 5..7 | 10..15) -> 1 | 4..7 | 10..15.
disjoint_stuff_4(3) -> 4; %% doesn't affect mismatching singleton and range
disjoint_stuff_4(X) -> X.

-spec var_pat(N :: 1..2, X :: 1..2) -> 2.
var_pat(1, X) -> 2;
var_pat(N, _) -> N.

-spec nil_elimination([atom()]) -> nonempty_list(atom()).
nil_elimination([]) -> [apa, bepa];
nil_elimination(Xs) -> Xs.

-spec tuple_union({a, b, c} | {a, b}) -> ok.
tuple_union({a, b, c}) -> ok;
tuple_union({a, b}) -> ok.

-spec beside_match_all(any(), a | b) -> b.
beside_match_all(_, a) -> b;
beside_match_all(_, X) -> X.

-spec beside_singleton(ignore, a | b) -> b.
beside_singleton(ignore, a) -> b;
beside_singleton(ignore, X) -> X.
