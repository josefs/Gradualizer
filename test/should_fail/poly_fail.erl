-module(poly_fail).


-export([find/0,
         use_hd/1,
         use_hd_var/1,
         use_map/1,
         use_map_var/1,
         use_hd_on_nonlist/1,
         map_constraint_error/1,
         map_constraint_error_var/1,
         use_app/1,
         use_app_var/1,
         use_id/1,
         use_id_var/1,
         filter_positive_ints/1,
         filter_positive_concrete_numbers/1,
         append_floats_to_ints/2,
         map_terms_to_binaries/1,
         my_list_to_integer2/0,
         my_list_to_integer3/0,
         my_list_to_integer/1,
         my_list_to_integer4/0,
         use_twice_contravariant1/2,
         use_twice_contravariant2/2,
         queue/2,
         infer_result_type/1,
         pass_multi_clause_fun/1,
         minus/1,
         invalid_case/2,
         invariant_tyvar/2,
         use_generic_hd/1,
         use_generic_hd_var/1,
         use_enum_map1/1,
         use_enum_map1_var/1,
         use_enum_map2/1,
         use_enum_map3/1
        ]).

-gradualizer([solve_constraints]).

%% The lookup/2 and find/0 functions come from the paper
%% "Bidirectional Typing for Erlang", N. Rajendrakumar, A. Bieniusa, 2021, Section 2. Examples.
-spec lookup(T1, [{T1, T2}]) -> (none | T2).
lookup(_, []) -> none;
lookup(K, [{K, V}|_]) -> V;
lookup(K, [_|KVs]) -> lookup(K, KVs).

-spec find() -> string().
find() ->
    %% This should fail type checking.
    "s" = lookup(0, [{0, 1}]).

-spec use_hd([integer(), ...]) -> atom().
use_hd(L) ->
    hd(L).

-spec use_hd_var([integer(), ...]) -> atom().
use_hd_var(L) ->
    Head = hd(L),
    Head.

-spec use_hd_on_nonlist(integer()) -> integer().
use_hd_on_nonlist(Int) ->
    hd(Int).

-spec use_map([integer()]) -> [atom()].
use_map(L) ->
    lists:map(fun times_two/1, L).

-spec use_map_var([integer()]) -> [atom()].
use_map_var(L) ->
    L2 = lists:map(fun times_two/1, L),
    L2.

-spec map_constraint_error([atom()]) -> [integer()].
map_constraint_error(L) ->
    lists:map(fun times_two/1, L).

-spec map_constraint_error_var([atom()]) -> [integer()].
map_constraint_error_var(L) ->
    L2 = lists:map(fun times_two/1, L),
    L2.

-spec times_two(integer()) -> integer().
times_two(I) -> I * 2.

-spec use_app(integer()) -> atom().
use_app(I) ->
    app(fun times_two/1, I).

-spec use_app_var(integer()) -> atom().
use_app_var(I) ->
    I2 = app(fun times_two/1, I),
    I2.

-spec app(fun ((A) -> B), A) -> B.
app(F, A) ->
    F(A).

-spec id(A) -> A.
id(X) -> X.

-spec use_id(atom()) -> integer().
use_id(Atom) ->
    id(Atom).

-spec use_id_var(atom()) -> integer().
use_id_var(Atom) ->
    X = id(Atom),
    X.

-spec positive(number()) -> boolean().
positive(N) -> N > 0.

-spec filter_positive_ints([integer()]) -> [float()].
filter_positive_ints(Ints) ->
    lists:filter(fun positive/1, Ints).

-spec filter_positive_concrete_numbers([1 | 2 | -3]) -> [1 | 4].
filter_positive_concrete_numbers(Ints) ->
    lists:filter(fun positive/1, Ints).

-spec append_floats_to_ints([float()], [integer()]) -> [integer()].
append_floats_to_ints(Floats, Ints) ->
    lists:append(Ints, Floats).

-spec map_terms_to_binaries([atom() | 42 | binary()]) -> string().
map_terms_to_binaries(Terms) ->
    lists:map(fun term_to_binary/1, Terms).

-spec compose(fun((A) -> B), fun((B) -> C), A) -> C.
compose(F, G, X) -> G(F(X)).

-spec compose_fun(fun((A) -> B), fun((B) -> C)) -> fun((A) -> C).
compose_fun(F, G) -> fun (X) -> G(F(X)) end.

-spec my_list_to_integer(atom()) -> integer().
my_list_to_integer(Atom) ->
    compose(fun list_to_binary/1, fun binary_to_integer/1, Atom).

-spec my_list_to_integer2() -> integer().
my_list_to_integer2() ->
    B2I = compose_fun(fun list_to_binary/1, fun binary_to_integer/1),
    B2I('123').

-spec my_list_to_integer3() -> fun((iolist() | atom()) -> integer()).
my_list_to_integer3() ->
    compose_fun(fun list_to_binary/1, fun binary_to_integer/1).

-spec my_list_to_integer4() -> any().
my_list_to_integer4() ->
    compose_fun(fun list_to_binary/1, fun integer_to_list/1).

-spec twice_contravariant(fun ((A) -> B), fun ((A) -> B)) -> fun ((A) -> B).
twice_contravariant(F, _G) -> F.

-spec use_twice_contravariant1(fun ((a | b | c) -> ok), fun ((b | c) -> ok)) -> fun ((a | b | c) -> ok).
use_twice_contravariant1(F, G) ->
    twice_contravariant(F, G).

-spec use_twice_contravariant2(fun ((a | b | c) -> ok), fun ((b | c) -> ok)) -> fun ((b | d) -> ok).
use_twice_contravariant2(F, G) ->
    twice_contravariant(F, G).

-spec queue(apple, banana) -> pineapple.
queue(Apple, Banana) ->
    Q1 = queue:new(),
    Q2 = queue:in(Apple, Q1),
    Q3 = queue:in(Banana, Q2),
    {{value, Value}, _Q4} = queue:out(Q3),
    Value.

-spec infer_result_type([integer()]) -> [atom()].
infer_result_type(Ints) ->
    lists:map(fun (I) -> times_two(I) end, Ints).

-spec multi_clause_fun(a) -> b;
           (c) -> d.
multi_clause_fun(a) -> b;
multi_clause_fun(c) -> d.

-spec pass_multi_clause_fun([a | c]) -> [b | x].
pass_multi_clause_fun(List) ->
    lists:map(fun multi_clause_fun/1, List).

-spec minus(5) -> -3.
minus(Five) ->
    X = id(Five),
    Y = -X,
    Y.

-spec invalid_case(atom(), number()) -> ok.
invalid_case(X, Y) ->
    Z = id(Y),
    case X of
        Z -> ok
    end.

-spec id_fun_arg(fun ((A) -> B),  A) -> {fun ((A) -> B), A}.
id_fun_arg(Fun, Arg) -> {Fun, Arg}.

-spec invariant_tyvar(number(), boolean()) -> any().
invariant_tyvar(Int, Bool) ->
    {Fun, _Arg} = id_fun_arg(fun positive/1, Int),
    Fun(Bool).

-spec generic_hd([A,...] | {A, any()}) -> A.
generic_hd([H | _]) -> H;
generic_hd({H, _}) -> H.

-spec use_generic_hd({atom(), integer()}) -> float().
use_generic_hd(Tuple) ->
    generic_hd(Tuple).

-spec use_generic_hd_var([atom(),...]) -> float().
use_generic_hd_var(List) ->
    X = generic_hd(List),
    X.

%% map/2 from Elixir's Enum module
-spec enum_map([A] | map(), fun ((A) -> B)) -> [B].
enum_map(List, Fun) when is_list(List) -> lists:map(Fun, List);
enum_map(_Struct, _Fun) -> throw(cannot_do_in_erlang).

-spec use_enum_map1([number()]) -> [float()].
use_enum_map1(Numbers) ->
    enum_map(Numbers, fun positive/1).

-spec use_enum_map1_var([number()]) -> [float()].
use_enum_map1_var(Numbers) ->
    X = enum_map(Numbers, fun positive/1),
    X.

-spec use_enum_map2([atom()]) -> [boolean()].
use_enum_map2(Atoms) ->
    enum_map(Atoms, fun positive/1).

-spec use_enum_map3(#{'__struct__' := some_struct}) -> [float()].
use_enum_map3(SomeStruct) ->
    enum_map(SomeStruct, fun positive/1).
