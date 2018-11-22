-module(type_refinement).

-export([basic_type_refinement/1,
         int_literals_1/1,
         int_literals_2/1,
         var_pat/2,
         nil_elimination/1]).

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

-spec var_pat(N :: 1..2, X :: 1..2) -> 2.
var_pat(1, X) -> 2;
var_pat(N, _) -> N.

-spec nil_elimination([atom()]) -> nonempty_list(atom()).
nil_elimination([]) -> [apa, bepa];
nil_elimination(Xs) -> Xs.
