-module(poly_should_fail).

-gradualizer([solve_constraints]).

-export([
    pass_id_to_takes_int_to_bool_fun/0,
    id_atom_is_int/1,
    id_id_atom_is_int/1,
    id_fun_id_atom_is_int/1,
    use_flatten/1,
    use_generic_hd/1,
    use_generic_hd_var/1,
    inference1/1,
    inference2/1
]).

-spec id(A) -> A.
id(X) -> X.

-spec id_fun(fun ((A) -> B)) -> fun ((A) -> B).
id_fun(Fun) -> Fun.

-spec takes_int_to_bool_fun(fun((integer()) -> boolean())) -> ok.
takes_int_to_bool_fun(_Fun) -> ok.

%% The following two functions demonstrate that we convert polymorphic
%% function types to function types with any() when using polymophic
%% functions outside of a call (directly, not via a variable).

-spec id_atom_is_int(atom()) -> integer().
id_atom_is_int(Atom) ->
    Id = fun id/1,
    Id(Atom).

-spec id_id_atom_is_int(atom()) -> integer().
id_id_atom_is_int(Atom) ->
    Id = id(fun id/1),
    Id(Atom).

-spec id_fun_id_atom_is_int(atom()) -> integer().
id_fun_id_atom_is_int(Atom) ->
    Id = id_fun(fun id/1),
    Id(Atom).

-spec pass_id_to_takes_int_to_bool_fun() -> ok.
pass_id_to_takes_int_to_bool_fun() ->
    takes_int_to_bool_fun(fun id/1).

%% Checking that [[atom()]] <: deep_list(A) where deep_list(A) :: [A | deep_list(A)]
%% returns the empty constraints because [[atom()]] can be a subtype of both A and
%% deep_list(A). This is related to the compat_ty(Ty, {type, union, _, _}) clause.
%% We would need a different representation of constraints to handle cases like this.
-spec use_flatten([[atom()]]) -> [integer()].
use_flatten(ListOfListsOfAtoms) ->
    lists:flatten(ListOfListsOfAtoms).

%% We do not support polymorphic intersection functions yet.
%% When calling intersection functions, type variables are replaced with any().

-spec generic_hd([A,...]) -> A;
                ({A, any()}) -> A.
generic_hd([H | _]) -> H;
generic_hd({H, _}) -> H.

-spec use_generic_hd({atom(), integer()}) -> float().
use_generic_hd(Tuple) ->
    generic_hd(Tuple).

-spec use_generic_hd_var([atom(),...]) -> float().
use_generic_hd_var(List) ->
    X = generic_hd(List),
    X.

%% We do not infer parameter types of anonymous functions (we assume any()).
%% We infer their result type only if we can derive it with parameter types set to any().

-spec inference1([string()]) -> [integer()].
inference1(L) ->
	lists:map(fun (I) -> I * 2 end, L).

-spec inference2([integer()]) -> [atom()].
inference2(L) ->
    lists:map(fun (I) -> I * 2 end, L).
