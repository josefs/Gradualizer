-module(poly_pass).

-export([use_hd/1,
         map_on_list_of_unions/0,
         queue/2,
         any_bound_propagation/2,
         pass_multi_clause_fun/1,
         use_id/1,
         use_id_var/1,
         use_id_via_var/1,
         filter_positive_ints/1,
         filter_positive_concrete_numbers/1,
         append_floats_to_ints/2,
         map_terms_to_binary/1,
         map_terms_to_binaries/1,
         my_list_to_integer/0,
         my_list_to_integer2/0,
         my_list_to_integer3/0,
         pass_id_to_takes_int_to_int_fun/0,
         use_twice_contravariant/2,
         maps_get_with_tuple/1,
         minus/1,
         plus/1,
         match_on_nth_result/1,
         invariant_tyvar1/1,
         invariant_tyvar2/1,
         use_enum_map1/1,
         use_enum_map2/1,
         find1/0
        ]).

-gradualizer([solve_constraints]).

-spec use_hd([integer(), ...]) -> integer().
use_hd(L) ->
    hd(L).

-type t1() :: {}.
-type t2() :: binary().
-type list_of_unions() :: [t1() | t2()].

-spec map_on_list_of_unions() -> [t1() | t2()].
map_on_list_of_unions() ->
    lists:map(fun id_for_t1_or_t2/1, return_list_of_unions([])).

-spec id_for_t1_or_t2(t1() | t2()) -> t1() | t2().
id_for_t1_or_t2(T) -> T.

-spec return_list_of_unions(list_of_unions()) -> list_of_unions().
return_list_of_unions(_L) -> [].

-spec queue(apple, banana) -> apple | banana.
queue(Apple, Banana) ->
    Q1 = queue:new(),
    Q2 = queue:in(Apple, Q1),
    Q3 = queue:in(Banana, Q2),
    {{value, Value}, _Q4} = queue:out(Q3),
    Value.

%% This tests that any() <: [A] results in the constraint {any() <: A}.
-spec any_bound_propagation(any(), integer()) -> boolean().
any_bound_propagation(Any, Int) ->
    List = lists:append(Any, [Int]),
    [AtLeastInt | _] = List,
    case AtLeastInt of
        1 -> true;
        atom -> false
    end.

-spec multi_clause_fun(a) -> b;
           (c) -> d.
multi_clause_fun(a) -> b;
multi_clause_fun(c) -> d.

-spec pass_multi_clause_fun([a | c]) -> [b | d].
pass_multi_clause_fun(List) ->
    lists:map(fun multi_clause_fun/1, List).

-spec id(A) -> A.
id(X) -> X.

-spec use_id(atom()) -> atom().
use_id(Atom) ->
    id(Atom).

-spec use_id_var(atom()) -> atom().
use_id_var(Atom) ->
    X = id(Atom),
    X.

-spec use_id_via_var(atom()) -> atom().
use_id_via_var(Atom) ->
    Id = fun id/1,
    Id(Atom).

-spec positive(number()) -> boolean().
positive(N) -> N > 0.

-spec filter_positive_ints([integer()]) -> [integer()].
filter_positive_ints(Ints) ->
    lists:filter(fun positive/1, Ints).

-spec filter_positive_concrete_numbers([1 | 2 | -3]) -> [1 | 2 | -3].
filter_positive_concrete_numbers(Ints) ->
    lists:filter(fun positive/1, Ints).

-spec append_floats_to_ints([float()], [integer()]) -> [number()].
append_floats_to_ints(Floats, Ints) ->
    lists:append(Ints, Floats).

-spec map_terms_to_binaries([atom() | 42 | binary()]) -> [binary()].
map_terms_to_binaries(Terms) ->
    lists:map(fun term_to_binary/1, Terms).

-spec concat([[A]]) -> [A].
concat([]) -> [];
concat([Xs | Ys]) -> Xs ++ concat(Ys).

-spec map_terms_to_binary([atom() | 42 | binary()]) -> binary().
map_terms_to_binary(Terms) ->
    Binaries = lists:map(fun term_to_binary/1, Terms),
    Strings = lists:map(fun binary_to_list/1, Binaries),
    String = concat(Strings),
    list_to_binary(String).

-spec compose(fun((A) -> B), fun((B) -> C), A) -> C.
compose(F, G, X) -> G(F(X)).

-spec compose_fun(fun((A) -> B), fun((B) -> C)) -> fun((A) -> C).
compose_fun(F, G) -> fun (X) -> G(F(X)) end.

-spec my_list_to_integer() -> integer().
my_list_to_integer() ->
    compose(fun list_to_binary/1, fun binary_to_integer/1, "123").

-spec my_list_to_integer2() -> integer().
my_list_to_integer2() ->
    B2I = compose_fun(fun list_to_binary/1, fun binary_to_integer/1),
    B2I("123").

-spec my_list_to_integer3() -> fun((iolist()) -> integer()).
my_list_to_integer3() ->
    compose_fun(fun list_to_binary/1, fun binary_to_integer/1).

-spec takes_int_to_int_fun(fun((integer()) -> integer())) -> ok.
takes_int_to_int_fun(_Fun) -> ok.

-spec pass_id_to_takes_int_to_int_fun() -> ok.
pass_id_to_takes_int_to_int_fun() ->
    takes_int_to_int_fun(fun id/1).

-spec twice_contravariant(fun ((A) -> B), fun ((A) -> B)) -> fun ((A) -> B).
twice_contravariant(F, _G) -> F.

-spec use_twice_contravariant(fun ((a | b | c) -> ok), fun ((b | c) -> ok)) -> fun ((b | c) -> ok).
use_twice_contravariant(F, G) ->
    twice_contravariant(F, G).

-spec maps_get_with_tuple(#{tuple := {a, b}}) -> ok.
maps_get_with_tuple(M) ->
    case maps:get(tuple, M, not_found) of
        not_found -> ok;
        {a, b} -> ok
    end.

-spec minus(5) -> -5.
minus(Five) ->
    X = id(Five),
    Y = -X,
    Y.

-spec plus(float()) -> float().
plus(Float) ->
    X = id(Float),
    Y = X + X,
    Y.

-spec match_on_nth_result(nonempty_list(#{atom() => integer()} | atom())) -> integer().
match_on_nth_result(L) ->
    V = lists:nth(2, L),
    case V of
        #{k := Int} ->
            Int;
        _ ->
            0
    end.

-spec id_fun_arg(fun ((A) -> B),  A) -> {fun ((A) -> B), A}.
id_fun_arg(Fun, Arg) -> {Fun, Arg}.

%% If a type variable is invariant in the result type and there are possible multiple substitutions,
%% there is no minimal substitution. Thus we choose substitution with `LowerBound | any()'.
%% The inferred type is in this case {fun ((integer() | any()) -> boolean()), integer() | any()}.
-spec invariant_tyvar1(integer()) -> {fun ((integer()) -> boolean()), integer()}.
invariant_tyvar1(Int) ->
    {Fun, Arg} = id_fun_arg(fun positive/1, Int),
    Fun(Int),
    {Fun, Arg}.

%% In this case, there is only one possible substitution (A = number()), so we do not loose
%% any precision here.
-spec invariant_tyvar2(number()) -> {fun ((number()) -> boolean()), number()}.
invariant_tyvar2(Int) ->
    {Fun, Arg} = id_fun_arg(fun positive/1, Int),
    Fun(Int),
    {Fun, Arg}.

%% map/2 from Elixir's Enum module
-spec enum_map([A] | map(), fun ((A) -> B)) -> [B].
enum_map(List, Fun) when is_list(List) -> lists:map(Fun, List);
enum_map(_Struct, _Fun) -> throw(cannot_do_in_erlang).

-spec use_enum_map1([number()]) -> [boolean()].
use_enum_map1(Numbers) ->
    enum_map(Numbers, fun positive/1).

-spec use_enum_map2(#{'__struct__' := some_struct}) -> [boolean()].
use_enum_map2(SomeStruct) ->
    enum_map(SomeStruct, fun positive/1).

-spec lookup(T1, [{T1, T2}]) -> (none | T2).
lookup(_, []) -> none;
lookup(K, [{K, V}|_]) -> V;
lookup(K, [_|KVs]) -> lookup(K, KVs).

-spec find1() -> string().
find1() ->
    case lookup(0, [{0, "s"}]) of
        none -> "default";
        V -> V
    end.
