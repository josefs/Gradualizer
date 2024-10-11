%% @private
-module(typechecker).

%% API used by gradualizer.erl
-export([type_check_forms/2]).

%% API used by the constraints module
-export([glb/2,
         lub/2]).

%% Functions used in unit tests.
-export([type_check_expr/2,
         type_check_expr_in/3,
         create_env/2,
         subtype/3,
         subtype_with_constraints/3,
         normalize/2,
         glb/3,
         type/1, type/2,
         type_diff/3,
         refinable/2,
         add_types_pats/4,
         compatible/3,
         collect_specs_types_opaques_and_functions/1,
         number_of_exported_functions/1,
         bounded_type_list_to_type/2,
         unfold_bounded_type/2,
         minimal_substitution/2,
         type_vars_variances/1]).

-compile([warn_missing_spec, warn_missing_spec_all,
          warnings_as_errors]).

-include("typelib.hrl").

-define(verbose(Env, Fmt, Args),
        case Env#env.verbose of
            true -> io:format(Fmt, Args);
            false -> ok
        end).

-define(throw_orig_type(EXPR, ORIGTYPE, NORMTYPE),
        try
            EXPR
        catch
            throw:TypeError:ST ->
                case error_evidence(TypeError) of
                    NORMTYPE ->
                        %% All error tuple sizes are > 0.
                        Index = size(TypeError),
                        Index > 0 orelse erlang:error(impossible),
                        Index = ?assert_type(size(TypeError), pos_integer()),
                        %% if the last element of the type_error tuple is the normalized type
                        %% replace it with the original result type
                        erlang:raise(throw, setelement(Index, TypeError, ORIGTYPE), ST);
                    _ ->
                        erlang:raise(throw, TypeError, ST)
                end
        end).

%% Checks that the location annotation of a type is set to zero and raises an
%% error if it isn't.
-define(assert_normalized_anno(Tuple),
        case erl_anno:location(element(2, Tuple)) of
            0 -> ok;
            _ -> error({position_not_removed, Tuple})
        end).

%% This is the maximum time that typechecking a single form may take.
-define(form_check_timeout_ms, 500).

-type venv() :: map().

-export_type([env/0,
              venv/0,
              type/0,
              typed_record_field/0]).

-type expr() :: gradualizer_type:abstract_expr().
-type pattern() :: gradualizer_type:abstract_pattern().
-type type() :: gradualizer_type:abstract_type().

-type form() :: erl_parse:abstract_form().
-type forms() :: [form()].

%% Pattern macros
-define(type(T), {type, _, T, []}).
-define(type(T, A), {type, _, T, A}).
-define(top(), {remote_type, _, [{atom,_,gradualizer}
				,{atom,_,top},[]]}).
-define(record_field(Name), {record_field, _, {atom, _, Name}, _}).
-define(record_field_expr(Expr), {record_field, _, _, Expr}).
-define(typed_record_field(Name), {typed_record_field, ?record_field(Name), _}).
-define(typed_record_field(Name, Type), {typed_record_field, ?record_field(Name), Type}).
-define(type_field_type(Name, Type), {type, _, field_type, [{atom, _, Name}, Type]}).
-define(any_assoc, ?type(map_field_assoc, [?type(any), ?type(any)])).
-define(user_type(), {user_type, _, _, _}).

%% Data collected from epp parse tree
-record(parsedata, {
          module             :: atom(),
          export_all = false :: boolean(),
          exports    = []    :: [{atom(), integer()}],
          imports    = []    :: [{module(), atom(), integer()}],
          specs      = []    :: list(),
          types      = []    :: list(),
          opaques    = []    :: list(),
          records    = []    :: list(),
          functions  = []    :: list()
         }).

-type af_field_name() :: gradualizer_type:af_field_name().
-type record_field() :: gradualizer_type:af_record_field(expr()).
-type record_field_type() :: gradualizer_type:af_record_field_type().
-type typed_record_field() :: {typed_record_field, record_field(), type()}.
-type af_assoc_type() :: gradualizer_type:af_assoc_type().

%% The environment passed around during typechecking.
%% TODO: See https://github.com/josefs/Gradualizer/issues/364 for details.
%%       Making the type def and record def have the same number of fields fixes a broken Gradualizer
%%       diagnostic, which seems to assume the record only has the
%%       fields annotated in the type, not all the fields from the definition.
-include("typechecker.hrl").
-type env() :: #env{}.

-include_lib("stdlib/include/assert.hrl").
-include("constraints.hrl").
-include("gradualizer.hrl").

-type anno() :: erl_anno:anno().
-type binary_op() :: gradualizer_type:binary_op().
-type bounded_function() :: gradualizer_type:af_constrained_function_type().
-type unary_op() :: gradualizer_type:unary_op().

%% TODO: Some of these don't seem to be thrown at all, e.g. expected_fun_type
-type type_error() :: arith_error | badkey | call_arity | call_intersect | check_clauses | cons_pat
                    | cyclic_type_vars | int_error | list | mismatch
                    | non_number_argument_to_minus
                    | non_number_argument_to_plus | op_type_too_precise | operator_pattern | pattern
                    | receive_after | record_pattern | rel_error | relop | unary_error
                    | unreachable_clauses.
-type undef() :: record | user_type | remote_type | record_field.

-type error() :: {type_error, type_error()}
               | {type_error, type_error(), anno()}
               | {type_error, expr(), type() | [type()], type()}
               | {type_error, type_error(), anno() | type(), type() | expr()}
               | {type_error, cyclic_type_vars, anno(), bounded_function(), list()}
               | {type_error, type_error(), anno(), atom() | pattern(), type()}
               | {type_error, type_error(), unary_op() | binary_op(), anno(), type()}
               | {type_error, type_error(), binary_op(), anno(), type(), type()}
               | {type_error, call_intersect, anno(), expr(), type(), [type()]}
               | {type_error, call_arity, anno(), atom(), arity(), arity()}
               | {undef, undef(), anno(), {atom(), atom() | non_neg_integer()} | mfa() | expr()}
               | {undef, undef(), expr()}
               | {not_exported, remote_type, anno(), mfa()}
               | {bad_type_annotation, gradualizer_type:af_string()}
               | {illegal_map_type, type()}
               | {argument_length_mismatch, anno(), arity(), arity()}
               | {nonexhaustive, anno(), [expr()]}
               | {illegal_pattern, pattern()}
               | {internal_error, missing_type_spec, atom(), arity()}
               | {call_undef, anno(), module(), atom(), arity()}
               | {constraint_error, atom(), type(), type(), expr(), type(), [type()]}.
%% `typechecker' returns these errors as results of its analysis.

%% Two types are compatible if one is a subtype of the other, or both.
-spec compatible(type(), type(), env()) -> boolean().
compatible(Ty1, Ty2, Env) ->
    case {subtype(Ty1, Ty2, Env), subtype(Ty2, Ty1, Env)} of
        {true, _} ->
            true;
        {_, true} ->
            true;
        {false, false} ->
            false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subtyping compatibility
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The first argument is a "compatible subtype" of the second.

-spec subtype(type(), type(), env()) -> boolean().
subtype(Ty1, Ty2, Env) ->
    case subtype_with_constraints(Ty1, Ty2, Env) of
        {true, _Cs} ->
            %% There shouldn't be any constraint really.
            %% We could assert that the constraints are empty but this could crash
            %% Gradualizer even for end users if there was some error in our code,
            %% it's probably safer not to assert and just drop the constraints
            true;
        false ->
            false
    end.

-spec subtype_with_constraints(type(), type(), env()) -> {true, constraints:t()} | false.
subtype_with_constraints(Ty1, Ty2, Env) ->
    Module = maps:get(module, Env#env.tenv),
    case gradualizer_cache:get(subtype, {Module, Ty1, Ty2}) of
        none ->
            R = try compat(Ty1, Ty2, maps:new(), Env) of
                    {_Memoization, Constraints} ->
                        {true, Constraints}
                catch
                    nomatch ->
                        false
                end,
            gradualizer_cache:store(subtype, {Module, Ty1, Ty2}, R),
            R;
        {some, R} ->
            %% these two types have already been seen and calculated
            R
    end.

%% Check if at least one of the types in a list is a subtype of a type.
%% Used when checking intersection types.
%%
%% When working on function intersections, `any_subtype/3' combines input parameter constraints
%% from multiple clauses into a single union type constraint.
-spec any_subtype([type()], type(), env()) -> boolean().
any_subtype(Tys, Ty, Env) ->
    any_subtype(Tys, Ty, Env, false).

-spec any_subtype([type()], type(), env(), boolean()) -> boolean().
any_subtype([], _Ty, _Env, true) ->
    true;
any_subtype([], _Ty, _Env, false) ->
    false;
any_subtype([Ty1|Tys], Ty, Env, AnySubtype) ->
    case subtype(Ty1, Ty, Env) of
        true ->
            any_subtype(Tys, Ty, Env, true);
        false ->
            any_subtype(Tys, Ty, Env, AnySubtype)
    end.

-type compat_acc() :: {Seen :: map(), constraints:t()}.

% This function throws an exception in case of a type error

%% The functions compat and compat_ty are mutually recursive.
%% The main entry point is compat and all recursive calls should go via compat.
%% The function compat_ty is just a convenience function to be able to
%% pattern match on types in a nice way.
-spec compat(type(), type(), map(), env()) -> compat_acc().
compat(T1, T2, Seen, Env) ->
    ?assert_normalized_anno(T1),
    ?assert_normalized_anno(T2),
    Ty1 = fixpoint_normalize(T1, Env),
    Ty2 = fixpoint_normalize(T2, Env),
    case compat_seen({T1, T2}, Seen) of
        true ->
            ret(Seen);
        false ->
            compat_ty(Ty1, Ty2, maps:put({T1, T2}, true, Seen), Env)
    end.

-spec fixpoint_normalize(type(), env()) -> type().
fixpoint_normalize(Ty, Env) ->
    NormTy = normalize(Ty, Env),
    case NormTy of
        Ty -> Ty;
        _ -> fixpoint_normalize(NormTy, Env)
    end.

%% Stop the recursion if we've already been queried whether T1 <: T2.
%%
%% This is because of recursive types. For example, suppose we have
%% two binary tree types defined like so:
%%   -type tree_int() :: leaf | {node, integer(), tree_int(), tree_int()}.
%%   -type tree_5() :: leaf | {node, 5, tree_5(), tree_5()}.
%% Now, if we want to check whether tree_5() <: tree_int(), we have to make
%% the following recursive calls:
%%   - leaf <: tree_int()
%%     - leaf <: leaf
%%   - {node, 5, tree_5(), tree_5()} <: tree_int()
%%     - {node, 5, tree_5(), tree_5()} <: leaf (fails)
%%     - {node, 5, tree_5(), tree_5()} <: {node, integer(), tree_int(), tree_int()}
%%       - node <: node
%%       - 5 <: integer()
%%       - tree_5() <: tree_int() <-- and this is the problem!
%%       - tree_5() <: tree_int() (for the right child; would cause the same problem)
%% If we didn't stop when querying for tree_5() <: tree_int() again,
%% we would have an infinite loop.
-spec compat_seen({type(), type()}, #{ {type(), type()} := true }) -> boolean().
compat_seen({T1, T2}, Seen) ->
    maps:get({T1, T2}, Seen, false).

-spec compat_ty(type(), type(), map(), env()) -> compat_acc().

%% Every type is subtype of itself
compat_ty(T, T, Seen, _Env) ->
    ret(Seen);

%% Variables
compat_ty({var, _, Var}, Ty, Seen, _Env) ->
    {Seen, constraints:upper(Var, Ty)};
compat_ty(Ty, {var, _, Var}, Seen, _Env) ->
    {Seen, constraints:lower(Var, Ty)};

%% any() and term() are used as the unknown type in the gradual type system
compat_ty({type, _, any, []} = Any, Type, Seen, Env) ->
    %% anything is a supertype of any(),
    %% the code below is just for case when Type contains type variables;
    TyVars = maps:to_list(type_vars_variances(Type)),
    Fun = fun
        %% for example when checking any() <: [A], we implicitly lift any() to [any()]
        %% (resulting in [any()] <: [A]) and record that any() must be the lower bound of A.
        ({Var, covariant}) -> constraints:lower(Var, Any);
        ({Var, contravariant}) -> constraints:upper(Var, Any);
        ({Var, invariant}) -> constraints:combine(constraints:lower(Var, Any),
                                                    constraints:upper(Var, Any), Env)
    end,
    Constraints = constraints:combine(lists:map(Fun, TyVars), Env),
    {Seen, Constraints};
compat_ty(Type, {type, _, any ,[]} = Any, Seen, Env) ->
    %% anything is a subtype of any(),
    %% the code below is just for case when Type contains type variables
    TyVars = maps:to_list(type_vars_variances(Type)),
    Fun = fun
        ({Var, covariant}) -> constraints:upper(Var, Any);
        ({Var, contravariant}) -> constraints:lower(Var, Any);
        ({Var, invariant}) -> constraints:combine(constraints:lower(Var, Any),
                                                    constraints:upper(Var, Any), Env)
    end,
    Constraints = constraints:combine(lists:map(Fun, TyVars), Env),
    {Seen, Constraints};

%% gradualizer:top() is the top of the subtyping hierarchy
compat_ty(_, ?top(), Seen, _Env) ->
    ret(Seen);

%% none() is the bottom of the subtyping relation
compat_ty({type, _, none, []}, _, Seen, _Env) ->
    ret(Seen);

% TODO: There are several kinds of fun types.
% Add support for them all eventually
compat_ty({type, _, 'fun', [{type, _, any}, Res1]},
          {type, _, 'fun', [{type, _, any}, Res2]},
          Seen, Env) ->
    %% We can assert the below, as we know Res1/Res2 are not {type, _, any},
    %% as they're not the first list element.
    compat(?assert_type(Res1, type()), ?assert_type(Res2, type()), Seen, Env);
compat_ty({type, _, 'fun', [{type, _, product, Args1}, Res1]},
          {type, _, 'fun', [{type, _, any}, Res2]},
          Seen, Env) ->
    Args2 = lists:duplicate(length(Args1), type(any)),
    % We check the argument types because Args1 may contain type variables
    % and in that case, we want to constrain them
    {Seen1, Cs1} = compat_tys(Args2, Args1, Seen, Env),
    {Seen2, Cs2} = compat(?assert_type(Res1, type()), ?assert_type(Res2, type()), Seen1, Env),
    {Seen2, constraints:combine(Cs1, Cs2, Env)};
compat_ty({type, _, 'fun', [{type, _, product, Args1}, Res1]},
          {type, _, 'fun', [{type, _, product, Args2}, Res2]},
          Seen, Env) ->
    {Ap, Cs} = compat_tys(Args2, Args1, Seen, Env),
    {Aps, Css} = compat(?assert_type(Res1, type()), ?assert_type(Res2, type()), Ap, Env),
    {Aps, constraints:combine(Cs, Css, Env)};

%% Unions
compat_ty({type, _, union, Tys1}, {type, _, union, Tys2}, Seen, Env) ->
    lists:foldl(fun (Ty1, {Seen1, C1}) ->
                    {Seen2, C2} = any_type(Ty1, Tys2, Seen1, Env),
                    {Seen2, constraints:combine(C1, C2, Env)}
                end, {Seen, constraints:empty()}, Tys1);
compat_ty(Ty1, {type, _, union, Tys2}, Seen, Env) ->
    any_type(Ty1, Tys2, Seen, Env);
compat_ty({type, _, union, Tys1}, Ty2, Seen, Env) ->
    all_type(Tys1, Ty2, Seen, Env);

% Integer types
compat_ty(Ty1, Ty2, Seen, _Env) when ?is_int_type(Ty1), ?is_int_type(Ty2) ->
    case gradualizer_int:is_int_subtype(Ty1, Ty2) of
        true -> ret(Seen);
        false -> throw(nomatch)
    end;

%% Atoms
compat_ty({atom, _, _Atom}, {type, _, atom, []}, Seen, _Env) ->
    ret(Seen);

%% Binary, bitstring
%%
%% <<_:M, _:_*N>> means
%% a bitstring with bit size M + K*N (for any K)
%%
%% <<_:M1, _:_*N1>> is subtype of <<_:M2, _:_*N2>> if
%% for all K, there is an L such that
%% M1 + K*N1 == M2 + L*N2
%%
%%                 M1      M1+N1   M1+2*N1
%% T1 .............|.......|.......|...
%% T2 .....|...|...|...|...|...|...|...
%%         M2      M2+L*N2
%%
compat_ty({type, _, binary, [{integer, _, M1}, {integer, _, N1}]},
          {type, _, binary, [{integer, _, M2}, {integer, _, N2}]},
          Seen, _Env)
  when N2 > 0, M1 >= M2,
       N1 rem N2 == 0,
       (M1 - M2) rem N2 == 0 ->
    ret(Seen);

%% Records with the same name, defined in different modules
%% TODO: Record equivallend on tuple form
compat_ty({type, P1, record, [{atom, _, Name}]},
          {type, P2, record, [{atom, _, Name}]}, Seen, Env) ->
    Fields1 = get_maybe_remote_record_fields(Name, P1, Env),
    Fields2 = get_maybe_remote_record_fields(Name, P2, Env),
    compat_record_fields(Fields1, Fields2, Seen, Env);

%% Records that have been refined on one side or the other
compat_ty({type, Anno1, record, [{atom, _, Name} | Fields1]},
          {type, Anno2, record, [{atom, _, Name} | Fields2]}, Seen, Env) ->
    AllFields1 = case Fields1 of [] -> get_record_fields_types(Name, Anno1, Env); _ -> Fields1 end,
    AllFields2 = case Fields2 of [] -> get_record_fields_types(Name, Anno2, Env); _ -> Fields2 end,
    compat_record_tys(AllFields1, AllFields2, Seen, Env);
compat_ty({type, _, record, _}, {type, _, tuple, any}, Seen, _Env) ->
    ret(Seen);

%% Lists
compat_ty(Ty1, Ty2, Seen, Env) when ?is_list_type(Ty1), ?is_list_type(Ty2) ->
    {Empty1, Elem1, Term1} = list_view(Ty1),
    {Empty2, Elem2, Term2} = list_view(Ty2),
    case {Empty1, Empty2} of
        {E, E}   -> ok;
        {_, any} -> ok;
        _        -> throw(nomatch)
    end,
    compat_tys([Elem1, Term1], [Elem2, Term2], Seen, Env);

%% Tuples
compat_ty({type, _, tuple, any}, {type, _, tuple, any}, Seen, _Env) ->
    ret(Seen);
compat_ty({type, _, tuple, any}, {type, _, tuple, Args2}, Seen, Env) ->
    Args1 = lists:duplicate(length(Args2), type(any)),
    % We check the argument types because Args2 may contain type variables
    % and in that case, we want to constrain them
    compat_tys(Args1, Args2, Seen, Env);
compat_ty({type, _, tuple, Args1}, {type, _, tuple, any}, Seen, Env) ->
    Args2 = lists:duplicate(length(Args1), type(any)),
    compat_tys(Args1, Args2, Seen, Env);
compat_ty({type, _, tuple, Args1}, {type, _, tuple, Args2}, Seen, Env) ->
    compat_tys(Args1, Args2, Seen, Env);

%% Maps
compat_ty({type, _, map, [?any_assoc]}, {type, _, map, _Assocs}, Seen, _Env) ->
    ret(Seen);
compat_ty({type, _, map, _Assocs}, {type, _, map, [?any_assoc]}, Seen, _Env) ->
    ret(Seen);
compat_ty({type, _, map, Assocs1}, {type, _, map, Assocs2}, Seen, Env) ->
    %% Please see: https://github.com/josefs/Gradualizer/wiki/Map-types#subtyping-rule-long-version
    %% for the below rule definitions:
    %% 2. For all mandatory associations K2 := V2 in M2,
    %% there is a mandatory association K1 := V1 in M1...
    IsMandatory = fun
                      ({type, _, map_field_exact, _}) -> true;
                      (_) -> false
                  end,
    MandatoryAssocs1 = lists:filter(IsMandatory, Assocs1),
    MandatoryAssocs2 = lists:filter(IsMandatory, Assocs2),
    {Seen3, Cs3} = lists:foldl(fun ({type, _, map_field_exact, _} = Assoc2, {Seen2, Cs2}) ->
                                    %% This nested loop will only throw nomatch, if there's at least
                                    %% one assoc in MandatoryAssocs1;
                                    %% if that's not the case, let's throw now.
                                    length(MandatoryAssocs1) == 0 andalso throw(nomatch),
                                    case lists:foldl(fun
                                                         (Assoc1, nomatch) ->
                                                             try
                                                                 compat(Assoc1, Assoc2, Seen2, Env)
                                                             catch
                                                                 nomatch -> nomatch
                                                             end;
                                                         (_Assoc1, {Seen1, Cs1}) ->
                                                             {Seen1, Cs1}
                                                     end, nomatch, MandatoryAssocs1)
                                    of
                                        nomatch -> throw(nomatch);
                                        {Seen1, Cs1} -> {Seen1, constraints:combine(Cs1, Cs2, Env)}
                                    end
                            end, ret(Seen), MandatoryAssocs2),
    %% 1. For all associations K1 <Assoc1> V1 in M1,
    %% there exists an association K2 <Assoc2> V2 in M2...
    lists:foldl(fun (Assoc1, {As, Cs1}) ->
                        {Ax, Cs2} = any_type(Assoc1, Assocs2, As, Env),
                        {Ax, constraints:combine(Cs1, Cs2, Env)}
                end, {Seen3, Cs3}, Assocs1);
compat_ty({type, _, AssocTag1, [Key1, Val1]},
          {type, _, AssocTag2, [Key2, Val2]}, Seen, Env)
        when AssocTag1 == map_field_assoc, AssocTag2 == map_field_assoc;
             AssocTag1 == map_field_exact, AssocTag2 == map_field_exact;
             AssocTag1 == map_field_exact, AssocTag2 == map_field_assoc ->
    %% For M1 <: M2, mandatory fields in M2 must be mandatory fields in M1
    {Seen1, Cs1} = compat(Key1, Key2, Seen, Env),
    {Seen2, Cs2} = compat(Val1, Val2, Seen1, Env),
    {Seen2, constraints:combine(Cs1, Cs2, Env)};

%% Opaque user types
compat_ty({user_type, Anno, Name, Args}, {user_type, Anno, Name, Args}, Seen, _Env) ->
    ret(Seen);
compat_ty({user_type, Anno, Name, Args1}, {user_type, Anno, Name, Args2}, Seen, Env)
  when length(Args1) == length(Args2) ->
    lists:foldl(fun ({Arg1, Arg2}, {Seen1, Cs1}) ->
                        {Seen2, Cs2} = compat(Arg1, Arg2, Seen1, Env),
                        {Seen2, constraints:combine(Cs1, Cs2, Env)}
                end, ret(Seen), lists:zip(Args1, Args2));

compat_ty(_Ty1, _Ty2, _, _) ->
    throw(nomatch).

-spec compat_tys([type()], [type()], map(), env()) -> compat_acc().
compat_tys([], [], Seen, _Env) ->
    ret(Seen);
compat_tys([Ty1|Tys1], [Ty2|Tys2], Seen, Env) ->
    {Seen1, Cs} = compat(Ty1 ,Ty2, Seen, Env),
    {Seen2, Css} = compat_tys(Tys1, Tys2, Seen1, Env),
    {Seen2, constraints:combine(Cs, Css, Env)};
compat_tys(_Tys1, _Tys2, _, _) ->
    throw(nomatch).


-spec compat_record_tys(list(), list(), map(), env()) -> compat_acc().
compat_record_tys([], [], Seen, _Env) ->
    ret(Seen);
compat_record_tys([?type_field_type(Name, Field1)|Fields1], [?type_field_type(Name, Field2)|Fields2], Seen, Env) ->
    {Seen1, Cs1} = compat(Field1, Field2, Seen, Env),
    {Seen2, Cs2} = compat_record_tys(Fields1, Fields2, Seen1, Env),
    {Seen2, constraints:combine(Cs1, Cs2, Env)};
compat_record_tys(_, _, _, _) ->
    %% Mismatching number of fields
    throw(nomatch).

%% Two records are compatible if they have the same name (defined in different
%% modules) and they have the same number of fields and the field types match.
-spec compat_record_fields([_], [_], map(), env()) -> compat_acc().
compat_record_fields([], [], Seen, _Env) ->
    ret(Seen);
compat_record_fields([{typed_record_field, _NameAndDefaultValue1, T1} | Fs1],
                     [{typed_record_field, _NameAndDefaultValue2, T2} | Fs2],
                     Seen, Env) ->
    {Seen1, Cs1} = compat(T1, T2, Seen, Env),
    {Seen2, Cs2} = compat_record_fields(Fs1, Fs2, Seen1, Env),
    {Seen2, constraints:combine(Cs1, Cs2, Env)};
compat_record_fields(_, _, _, _) ->
    %% Mismatching number of fields
    throw(nomatch).

%% Returns a successful matching of two types. Convenience function for when
%% there were no type variables involved.
-spec ret(map()) -> compat_acc().
ret(Seen) ->
    {Seen, constraints:empty()}.

%% Checks whether Ty is a subtype of at least one type from Tys.
-spec any_type(type(), [type()], map(), env()) -> compat_acc().
any_type(Ty1, Tys, Seen0, Env) ->
    F = fun (Ty2, {Seen1, Css}) ->
        try
            {Seen2, Cs} = compat(Ty1, Ty2, Seen1, Env),
            {Seen2, [Cs | Css]}
        catch
            nomatch ->
                {Seen1, Css}
        end
    end,
    {Seen3, Css} = lists:foldl(F, {Seen0, []}, Tys),
    case Css of
        [] ->
            %% No type from Tys is a supertype of Ty1.
            throw(nomatch);
        [Cs] ->
            %% Exactly one type from Tys is a supertype of Ty1.
            %% In this case we can return constraints, as no other type from Tys matches.
            {Seen3, Cs};
        _ ->
            %% Multiple types from Tys are supertypes of Ty1.
            %% TODO: Don't drop the constraint here.
            %% In this case we don't know how to constrain involved type variables,
            %% as for example in the call any_type(atom(), [A, B], ...) where
            %% A and B are type variables, both constraints {atom() <: A} and
            %% {atom() <: B} lead to a solution where atom() <: A | B.
            %% To express this situation, we would need a radically different
            %% representation of constraints, one which would allow to represent
            %% unions of constraints.
            {Seen3, constraints:empty()}
    end.

%% @doc All types in `Tys' must be compatible with `Ty'.
%% Returns all the gather memoizations and constraints.
%% Does not return (throws `nomatch') if any of the types is not compatible.
-spec all_type([type()], type(), map(), env()) -> compat_acc().
all_type(Tys, Ty, Seen, Env) ->
    all_type(Tys, Ty, Seen, [], Env).

-spec all_type([type()], type(), map(), [constraints:t()], env()) -> compat_acc().
all_type([], _Ty, Seen, Css, Env) ->
    {Seen, constraints:combine(Css, Env)};
all_type([Ty1|Tys], Ty, AIn, Css, Env) ->
    {AOut, Cs} = compat(Ty1, Ty, AIn, Env),
    all_type(Tys, Ty, AOut, [Cs|Css], Env).

%% Looks up the fields of a record by name and, if present, by the module where
%% it belongs if a filename is included in the Anno.
-spec get_maybe_remote_record_fields(RecName :: atom(),
                                     Anno    :: erl_anno:anno(),
                                     Env     :: env()) ->
                                            [{typed_record_field, _, type()}].
get_maybe_remote_record_fields(RecName, Anno, Env) ->
    case typelib:get_module_from_annotation(Anno) of
        {ok, Module} ->
            %% A record type in another module, from an expanded remote type
            case gradualizer_db:get_record_type(Module, RecName) of
                {ok, TypedRecordFields} ->
                    TypedRecordFields;
                not_found ->
                    throw(undef(record, Anno, {Module, RecName}))
            end;
        none ->
            %% Local record type
            get_record_fields(RecName, Anno, Env)
    end.

%% Looks up a record in the supplied type environment and returns its typed
%% fields.
-spec get_record_fields(RecName, Anno, Env) -> [typed_record_field()] when
      RecName :: atom(),
      Anno    :: erl_anno:anno(),
      Env     :: env().
get_record_fields(RecName, _Anno, #env{tenv = #{records := REnv}}) ->
    maps:get(RecName, REnv). % It must exist. Otherwise it's a compile error.

%% Greatest lower bound
%% --------------------
%%
%% * Computes the maximal (in the subtyping hierarchy) type that is a subtype
%%   of two given types.

-spec glb(type(), type(), env()) -> type().
glb(T1, T2, Env) ->
    glb(T1, T2, #{}, Env).

-spec glb([type()], env()) -> type().
glb(Ts, Env) ->
    lists:foldl(fun (T, TyAcc) -> glb(T, TyAcc, Env) end, top(), Ts).

-spec glb(type(), type(), map(), env()) -> type().
glb(T1, T2, A, Env) ->
    case stop_glb_recursion(T1, T2, A) of
        %% If we hit a recursive case we approximate with none(). Conceivably
        %% you could do some fixed point iteration here, but let's wait for an
        %% actual use case.
        true -> type(none);
        false ->
            Module = maps:get(module, Env#env.tenv),
            case gradualizer_cache:get(?FUNCTION_NAME, {Module, T1, T2}) of
                none ->
                    Ty1 = normalize(T1, Env),
                    Ty2 = normalize(T2, Env),
                    Ty = glb_ty(Ty1, Ty2, A#{ {T1, T2} => 0 }, Env),
                    NormTy = normalize(Ty, Env),
                    gradualizer_cache:store(?FUNCTION_NAME, {Module, T1, T2}, NormTy),
                    NormTy;
                {some, Ty} ->
                    %% these two types have already been seen and calculated
                    Ty
            end
    end.

%% A standalone function is easier to debug / trace.
-spec stop_glb_recursion(type(), type(), #{ {type(), type()} := any() }) -> boolean().
stop_glb_recursion(T1, T2, A) ->
    maps:is_key({T1, T2}, A).

-spec glb_ty(type(), type(), map(), env()) -> type().
%% none() is the bottom of the hierarchy
glb_ty({type, _, none, []} = Ty1, _Ty2, _A, _Env) ->
    Ty1;
glb_ty(_Ty1, {type, _, none, []} = Ty2, _A, _Env) ->
    Ty2;

%% We don't know anything if either type is any()
glb_ty({type, _, any, []} = Ty1, _Ty2, _A, _Env) ->
    Ty1;
glb_ty(_Ty1, {type, _, any, []} = Ty2, _A, _Env) ->
    Ty2;

%% gradualizer:top() is the top of the hierarchy
glb_ty(?top(), Ty2, _A, _Env) ->
    Ty2;
glb_ty(Ty1, ?top(), _A, _Env) ->
    Ty1;

%% glb is idempotent
glb_ty(Ty, Ty, _A, _Env) ->
    Ty;

%% Union types: glb distributes over unions
glb_ty({type, Ann, union, Ty1s}, Ty2, A, Env) ->
    Tys = [ glb(Ty1, Ty2, A, Env) || Ty1 <- Ty1s ],
    {type, Ann, union, Tys};
glb_ty(Ty1, {type, Ann, union, Ty2s}, A, Env) ->
    Tys = [ glb(Ty1, Ty2, A, Env) || Ty2 <- Ty2s ],
    {type, Ann, union, Tys};

%% Atom types
glb_ty(Ty1 = {atom, _, _}, {type, _, atom, []}, _A, _Env) ->
    Ty1;
glb_ty({type, _, atom, []}, Ty2 = {atom, _, _}, _A, _Env) ->
    Ty2;

%% Number types
glb_ty(Ty1, Ty2, _A, _Env) when ?is_int_type(Ty1), ?is_int_type(Ty2) ->
    gradualizer_int:int_type_glb(Ty1, Ty2);

%% List types
glb_ty(Ty1, Ty2, A, Env) when ?is_list_type(Ty1), ?is_list_type(Ty2) ->
    {Empty1, Elem1, Term1} = list_view(Ty1),
    {Empty2, Elem2, Term2} = list_view(Ty2),
    Empty =
        case {Empty1, Empty2} of
            {E, E}            -> E;
            {any, E}          -> E;
            {E, any}          -> E;
            {empty, nonempty} -> none;
            {nonempty, empty} -> none
        end,
    Elem = glb(Elem1, Elem2, A, Env),
    Term = glb(Term1, Term2, A, Env),
    from_list_view({Empty, Elem, Term});

%% Tuple types
glb_ty({type, _, tuple, any}, {type, _, tuple, _} = Ty2, _A, _Env) ->
    Ty2;
glb_ty({type, _, tuple, _} = Ty1, {type, _, tuple, any}, _A, _Env) ->
    Ty1;
glb_ty({type, _, tuple, Tys1}, {type, _, tuple, Tys2}, A, Env) ->
    Tys1 = ?assert_type(Tys1, [type()]),
    Tys2 = ?assert_type(Tys2, [type()]),
    EqualSizes = length(Tys1) =:= length(Tys2),
    if
        not EqualSizes ->
            type(none);
        EqualSizes ->
            Tys = lists:zipwith(fun(T1, T2) -> glb(T1, T2, A, Env) end, Tys1, Tys2),
            case lists:any(fun(?type(none)) -> true; (_) -> false end, Tys) of
                true ->
                    type(none);
                false ->
                    type(tuple, Tys)
            end
    end;

%% Record types. Either exactly the same record (handled above) or tuple().
glb_ty(Ty1 = {type, _, record, _}, {type, _, tuple, any}, _A, _Env) ->
    Ty1;
glb_ty({type, _, tuple, any}, Ty2 = {type, _, record, _}, _A, _Env) ->
    Ty2;
glb_ty({type, _, record, _}, {type, _, record, _}, _A, _Env) ->
    type(none);

%% Map types. These are a bit tricky.
%% For now going with a very crude approximation.
glb_ty(Ty1 = {type, _, map, Assocs1}, Ty2 = {type, _, map, Assocs2}, A, Env) ->
    case {Assocs1, Assocs2} of
        %% TODO: add a test case
        {[?any_assoc], _} -> Ty2;
        {_, [?any_assoc]} -> Ty1;
        _ ->
            %% TODO: Too simplistic!
            %% We're not capable of handling overlapping keys without intersection
            %% and negation types!
            case {has_overlapping_keys(Ty1, Env), has_overlapping_keys(Ty2, Env)} of
                {false, false} ->
                    NewAssocs0 = [ glb(As1, As2, A, Env) || As1 <- Assocs1,
                                                                   As2 <- Assocs2 ],
                    NewAssocs = lists:filter(fun(?type(none)) -> false; (_) -> true end, NewAssocs0),
                    case NewAssocs of
                        [] ->
                            type(none);
                        [_|_] ->
                            type(map, NewAssocs)
                    end;
                _ ->
                    type(none)
            end
    end;
glb_ty(?type(AssocTag1, [Key1, Val1]), ?type(AssocTag2, [Key2, Val2]), A, Env)
  when AssocTag1 == map_field_assoc, AssocTag2 == map_field_assoc;
       AssocTag1 == map_field_exact, AssocTag2 == map_field_exact;
       AssocTag1 == map_field_exact, AssocTag2 == map_field_assoc;
       AssocTag1 == map_field_assoc, AssocTag2 == map_field_exact ->

    Key1 = ?assert_type(Key1, type()),
    Val1 = ?assert_type(Val1, type()),
    Key2 = ?assert_type(Key2, type()),
    Val2 = ?assert_type(Val2, type()),

    AssocTag = case {AssocTag1, AssocTag2} of
                   {map_field_exact, map_field_exact} -> map_field_exact;
                   {map_field_exact, map_field_assoc} -> map_field_exact;
                   {map_field_assoc, map_field_exact} -> map_field_exact;
                   {map_field_assoc, map_field_assoc} -> map_field_assoc
               end,

    Key = case {Key1, AssocTag, Key2} of
              {?type(any), map_field_assoc, ?type(any)} -> type(any);
              {_, map_field_exact, ?type(any)} -> Key1;
              {?type(any), map_field_exact, _} -> Key2;
              {_, _, _} -> glb(Key1, Key2, A, Env)
              %{_, _, _} -> type(none)
          end,

    Val = case {Val1, AssocTag, Val2} of
              {?type(any), map_field_assoc, ?type(any)} -> type(any);
              {_, map_field_exact, ?type(any)} -> Val1;
              {?type(any), map_field_exact, _} -> Val2;
              {_, _, _} -> glb(Val1, Val2, A, Env)
              %{_, _, _} -> type(none)
          end,

    case lists:any(fun(?type(none)) -> true; (_) -> false end, [Key, Val]) of
        true ->
            type(none);
        false ->
            type(AssocTag, [Key, Val])
    end;

%% Binary types. For now approximate this by returning the smallest type if
%% they are comparable, otherwise none(). See the corresponding case in
%% compat_ty for the subtyping rule.
glb_ty(Ty1 = {type, _, binary, _},
       Ty2 = {type, _, binary, _}, _A, Env) ->
    case subtype(Ty1, Ty2, Env) of
        true -> Ty1;    %% Will never produce constraints
        false ->
            case subtype(Ty2, Ty1, Env) of
                true -> Ty2;
                false     -> type(none)
            end
    end;

%% Function types. Would require lub on arguments for proper implementation.
%% For now pick biggest arguments when comparable and none() otherwise.
glb_ty({type, _, 'fun', [{type, _, product, Args1}, Res1]},
       {type, _, 'fun', [{type, _, product, Args2}, Res2]}, A, Env) ->
    Res1 = ?assert_type(Res1, type()),
    Res2 = ?assert_type(Res2, type()),
    NoConstraints = constraints:empty(),
    Res = glb(Res1, Res2, A, Env),
    Subtype =
        fun(Ts1, Ts2) ->
            try compat_tys(Ts1, Ts2, maps:new(), Env) of
                {_, NoConstraints} -> true;
                _ -> false
            catch throw:nomatch -> false end
        end,
    case Subtype(Args1, Args2) of
        true  -> type('fun', [type(product, Args2), Res]);
        false ->
            case Subtype(Args2, Args1) of
                true  -> type('fun', [type(product, Args1), Res]);
                false -> type(none)
            end
    end;
glb_ty({type, _, 'fun', [{type, _, any} = Any, Res1]},
       {type, _, 'fun', [{type, _, any}, Res2]}, A, Env) ->
    Res1 = ?assert_type(Res1, type()),
    Res2 = ?assert_type(Res2, type()),
    Res = glb(Res1, Res2, A, Env),
    type('fun', [Any, Res]);

glb_ty({type, _, 'fun', [{type, _, any}, Res1]},
       {type, _, 'fun', [{type, _, product, _} = TArgs2, _]} = T2, A, Env) ->
    glb(type('fun', [TArgs2, Res1]), T2, A, Env);
glb_ty({type, _, 'fun', [{type, _, product, _} = TArgs1, _]} = T1,
       {type, _, 'fun', [{type, _, any}, Res2]}, A, Env) ->
    glb(T1, type('fun', [TArgs1, Res2]), A, Env);

%% normalize only does the top layer
glb_ty({type, _, Name, Args1}, {type, _, Name, Args2}, A, Env) ->
    Args1 = ?assert_type(Args1, [type()]),
    Args2 = ?assert_type(Args2, [type()]),
    if
        length(Args1) == length(Args2) ->
            Args = [ glb(Arg1, Arg2, A, Env) || {Arg1, Arg2} <- lists:zip(Args1, Args2) ],
            type(Name, Args);
        length(Args1) /= length(Args2) ->
            %% Incompatible
            type(none)
    end;

%% Incompatible
glb_ty(_Ty1, _Ty2, _A, _Env) ->
    type(none).

-spec has_overlapping_keys(type(), env()) -> boolean().
has_overlapping_keys({type, _, map, Assocs}, Env) ->
    Cart = [ case {subtype(As1, As2, Env), subtype(As2, As1, Env)} of
                 {false, false} ->
                     false;
                 {_R1, _R2} ->
                     true
             end
             || As1 <- ?assert_type(Assocs, list()),
                As2 <- ?assert_type(Assocs, list()),
                As1 /= As2 ],
    lists:any(fun(X) -> X end, Cart).

%% Non-singleton required (alias exact) keys are hard to work with
%% because they express that at least one value of that type should
%% be present in the map, but not necessarily all of them.
-spec has_non_singleton_required_keys(gradualizer_type:af_map_type()) -> boolean().
has_non_singleton_required_keys(?type(map, any)) -> false;
has_non_singleton_required_keys(?type(map, Assocs)) ->
    lists:any(
        fun(?type(map_field_exact, [KeyTy, _ValTy])) -> not is_singleton_type(KeyTy);
           (?type(map_field_assoc, _)) -> false
        end,
        Assocs
    ).

-spec lub([type()], env()) -> type().
lub(Tys, Env) ->
    normalize(type(union, Tys), Env).


%% Normalize
%% ---------
%%
%% * Expand user-defined and remote types on head level (except opaque types)
%% * Replace built-in type synonyms
%% * Flatten unions and merge overlapping types (e.g. ranges) in unions
-spec normalize(type(), env()) -> type().
normalize(Ty, Env) ->
    normalize_rec(Ty, Env).

%% The third argument is a set of user types that we've already unfolded.
%% It's important that we don't keep unfolding such types because it will
%% lead to infinite recursion.
-spec normalize_rec(type(), env()) -> type().
normalize_rec({type, _, union, Tys}, Env) ->
    UnionSizeLimit = Env#env.union_size_limit,
    Types = flatten_unions(Tys, Env),
    case merge_union_types(Types, Env) of
        []  -> type(none);
        [T] -> T;
        %% Performance hack: Unions larger than this value are replaced by any().
        Ts when length(Ts) > UnionSizeLimit -> type(any);
        Ts  -> type(union, Ts)
    end;
normalize_rec({user_type, _, Name, Args} = Type, Env) ->
    case gradualizer_lib:get_type_definition(Type, Env, []) of
        {ok, T} ->
            T1 = replace_type_vars_with_any(T),
            T2 = normalize_rec(T1, Env),
            T2;
        opaque ->
            Type;
        not_found ->
            P = position_info_from_spec(Env#env.current_spec),
            throw(undef(user_type, P, {Name, arity(length(Args))}))
    end;
normalize_rec(T = ?top(), _Env) ->
    %% Don't normalize gradualizer:top().
    T;
normalize_rec({remote_type, _, [{atom, _, M}, {atom, _, N}, Args]}, Env) ->
    %% It's safe as we explicitly match out `Module :: af_atom()' and `TypeName :: af_atom()'.
    Args = ?assert_type(Args, [type()]),
    P = position_info_from_spec(Env#env.current_spec),
    case gradualizer_db:get_exported_type(M, N, Args) of
        {ok, T} ->
            T1 = replace_type_vars_with_any(T),
            T2 = normalize_rec(T1, Env),
            T2;
        opaque ->
            NormalizedArgs = lists:map(fun (Ty) -> normalize_rec(Ty, Env) end, Args),
            Ty = {user_type, erl_anno:new(0), N, NormalizedArgs},
            typelib:annotate_user_type(M, ?assert_type(Ty, type()));
        not_exported ->
            throw(not_exported(remote_type, P, {M, N, arity(length(Args))}));
        not_found ->
            throw(undef(remote_type, P, {M, N, arity(length(Args))}))
    end;
normalize_rec({op, _, _, _Arg} = Op, _Env) ->
    erl_eval:partial_eval(Op);
normalize_rec({op, _, _, _Arg1, _Arg2} = Op, _Env) ->
    erl_eval:partial_eval(Op);
normalize_rec({type, Ann, range, [T1, T2]}, Env) ->
    %% We can assert that T1 and T2 are valid type() instances, because they must be expressed in
    %% Erlang type syntax. Infinities are not allowed there - it's a Gradualizer extension to allow
    %% them as range bounds.
    {type, Ann, range, [normalize_rec(?assert_type(T1, type()), Env),
                        normalize_rec(?assert_type(T2, type()), Env)]};
normalize_rec(Type, _Env) ->
    expand_builtin_aliases(Type).

%% Replace built-in type aliases
-spec expand_builtin_aliases(type()) -> type().
expand_builtin_aliases({var, Ann, '_'}) ->
    {type, Ann, any, []};
expand_builtin_aliases({type, Ann, term, []}) ->
    {type, Ann, any, []};
expand_builtin_aliases({type, Ann, binary, []}) ->
    {type, Ann, binary, [{integer, Ann, 0}, {integer, Ann, 8}]};
expand_builtin_aliases({type, Ann, nonempty_binary, []}) ->
    {type, Ann, binary, [{integer, Ann, 8}, {integer, Ann, 8}]};
expand_builtin_aliases({type, Ann, bitstring, []}) ->
    {type, Ann, binary, [{integer, Ann, 0}, {integer, Ann, 1}]};
expand_builtin_aliases({type, Ann, nonempty_bitstring, []}) ->
    {type, Ann, binary, [{integer, Ann, 1}, {integer, Ann, 1}]};
expand_builtin_aliases({type, Ann, boolean, []}) ->
    {type, Ann, union, [{atom, Ann, false}, {atom, Ann, true}]};
expand_builtin_aliases({type, Ann, bool, []}) ->
    {type, Ann, union, [{atom, Ann, false}, {atom, Ann, true}]};
expand_builtin_aliases({type, Ann, byte, []}) ->
    {type, Ann, range, [{integer, Ann, 0}, {integer, Ann, 255}]};
expand_builtin_aliases({type, Ann, char, []}) ->
    {type, Ann, range, [{integer, Ann, 0}, {integer, Ann, 16#10ffff}]};
expand_builtin_aliases({type, Ann, number, []}) ->
    {type, Ann, union, [{type, Ann, integer, []}, {type, Ann, float, []}]};
expand_builtin_aliases({type, Ann, list, []}) ->
    {type, Ann, list, [{type, Ann, any, []}]};
expand_builtin_aliases({type, Ann, maybe_improper_list, []}) ->
    {type, Ann, maybe_improper_list, [{type, Ann, any, []},
                                      {type, Ann, any, []}]};
expand_builtin_aliases({type, Ann, nonempty_list, []}) ->
    {type, Ann, nonempty_list, [{type, Ann, any, []}]};
expand_builtin_aliases({type, Ann, string, []}) ->
    {type, Ann, list, [{type, Ann, char, []}]};
expand_builtin_aliases({type, Ann, nonempty_string, []}) ->
    {type, Ann, nonempty_list, [{type, Ann, char, []}]};
expand_builtin_aliases({type, Ann, iodata, []}) ->
    {type, Ann, union, [{type, Ann, iolist, []}, {type, Ann, binary, []}]};
expand_builtin_aliases({type, Ann, iolist, []}) ->
    %% recursive type
    Union = [{type, Ann, byte, []},
             {type, Ann, binary, []},
             {type, Ann, iolist, []}],
    Tail = [{type, Ann, nil, []},
            {type, Ann, binary, []}],
    {type, Ann, maybe_improper_list, [{type, Ann, union, Union},
                                      {type, Ann, union, Tail}]};
expand_builtin_aliases({type, Ann, map, any}) ->
    {type, Ann, map, [{type, Ann, map_field_assoc, [{type, Ann, any, []},
                                                    {type, Ann, any, []}]}]};
expand_builtin_aliases({type, Ann, function, []}) ->
    {type, Ann, 'fun', [{type, Ann, any}, {type, Ann, any, []}]};
expand_builtin_aliases({type, Ann, 'fun', []}) ->
    %% `fun()' is not a built-in alias per the erlang docs
    %% but it is equivalent with `fun((...) -> any())'
    {type, Ann, 'fun', [{type, Ann, any}, {type, Ann, any, []}]};
expand_builtin_aliases({type, Ann, module, []}) ->
    {type, Ann, atom, []};
expand_builtin_aliases({type, Ann, mfa, []}) ->
    {type, Ann, tuple, [{type, Ann, module, []},
                        {type, Ann, atom, []},
                        {type, Ann, arity, []}]};
expand_builtin_aliases({type, Ann, arity, []}) ->
    {type, Ann, range, [{integer, Ann, 0}, {integer, Ann, 255}]};
expand_builtin_aliases({type, Ann, identifier, []}) ->
    {type, Ann, union, [{type, Ann, pid, []},
                        {type, Ann, port, []},
                        {type, Ann, reference, []}]};
expand_builtin_aliases({type, Ann, node, []}) ->
    {type, Ann, atom, []};
expand_builtin_aliases({type, Ann, timeout, []}) ->
    {type, Ann, union, [{atom, Ann, infinity},
                        {type, Ann, non_neg_integer, []}]};
expand_builtin_aliases({type, Ann, no_return, []}) ->
    {type, Ann, none, []};
%% TODO: This is a kludge by which lists of types slip through calls to normalize().
%%       Specifically, lists of Types representing bounded fun clauses.
expand_builtin_aliases(Type) ->
    Type.

%% Flattens nested unions
%%
%% Mutually recursive with normalize/2
%%
%% * Normalize each element
%% * TODO: Detect user-defined and remote types expanding to themselves
%%   or to unions containing themselves (using memoization?)
%% * Remove subtypes of other types in the same union; keeping any() separate
%% * Merge integer types, including singleton integers and ranges
%%   1, 1..5, integer(), non_neg_integer(), pos_integer(), neg_integer()
-spec flatten_unions([type()], env()) -> [type()].
flatten_unions(Tys, Env) ->
    lists:flatmap(fun (Ty) -> flatten_union(Ty, Env) end, Tys).

-spec flatten_union(type(), env()) -> [type()].
flatten_union({user_type, _, _, _} = Ty, _Env) ->
    [Ty];
flatten_union({type, _, none, []}, _Env) ->
    [];
flatten_union({type, _, no_return, []}, _Env) ->
    %% This is an alias for `none()' which should've been removed by `expand_builtin_aliases',
    %% but there's no guarantee the latter has been called by now.
    %% If it wasn't, `fixpoint_normalize' might alternate between a union with `none()'
    %% and without it and never stop.
    %% Caught thanks to `prop_compatible' test.
    [];
flatten_union({type, _, union, Tys}, Env) ->
    flatten_unions(Tys, Env);
flatten_union(Ty, Env) ->
    [normalize_rec(Ty, Env)].

%% Merges overlapping integer types (including ranges and singletons).
%% (TODO) Removes all types that are subtypes of other types in the same union.
%% Returns a list of disjoint types.
-spec merge_union_types([type()], env()) -> [type()].
merge_union_types(Types, _Env) ->
    case lists:any(fun is_top/1, Types) of
        true ->
            %% gradualizer:top() is among the types.
            [top()];
        false ->
            {IntegerTypes1, OtherTypes1} =
                lists:partition(fun gradualizer_int:is_int_type/1, Types),
            IntegerTypes2 = gradualizer_int:merge_int_types(IntegerTypes1),
            OtherTypes2 = merge_atom_types(OtherTypes1),
            OtherTypes3 = lists:usort(OtherTypes2),
            IntegerTypes2 ++ OtherTypes3
    end.

-spec is_top(gradualizer:top()) -> boolean().
is_top(?top()) -> true;
is_top(_) -> false.

%% Decides whether the type expresses exactly one value.
-spec is_singleton_type(type()) -> boolean().
is_singleton_type({atom, _, _Atom}) -> true;
is_singleton_type({integer, _, _Integer}) -> true;
is_singleton_type({char, _, _Char}) -> true;
is_singleton_type(?type(range, [{integer, _, N}, {integer, _, N}])) -> true;
is_singleton_type(?type(tuple, [])) -> true;
is_singleton_type(?type(map, [])) -> true;
is_singleton_type(?type(nil)) -> true;
is_singleton_type(?type(binary, [{integer, _, 0},{integer, _, 0}])) -> true; % empty binary
is_singleton_type(_) -> false. % TODO: add more cases

%% Remove all atom listerals if atom() is among the types.
-spec merge_atom_types([type()]) -> [type()].
merge_atom_types(Types) ->
    IsAnyAtom = lists:any(fun ({type, _, atom, []}) -> true;
                              (_)                   -> false
                          end,
                          Types),
    if
        IsAnyAtom -> lists:filter(fun ({atom, _, _}) -> false;
                                      (_)            -> true
                                  end,
                                  Types);
        true      -> Types
    end.



%% Input arg must be already normalized!
%% That being said, we still accept Env in case of other options that might be stored there.
-spec negate_num_type(type(), env()) -> type().
negate_num_type({type, _, TyName, []} = Ty, _Env) when
      TyName =:= any;
      TyName =:= integer;
      TyName =:= float ->
    Ty;
negate_num_type({var,  _, _} = Ty, _Env) ->
    Ty;
negate_num_type({integer, P, I}, _Env) ->
    {integer, P, -I};
negate_num_type({type, P, union, Tys}, Env) ->
    %% We normalize the result only to merge `0 | pos_integer()` =>
    %% `non_neg_integer()` and to have a nice increasing order of Tys.
    %% The incoming union type must be already normalized so it shouldn't
    %% contain any unresolved types. So it is ok to normalize the result with an
    %% empty TEnv.
    normalize({type, P, union, [negate_num_type(Ty, Env) || Ty <- Tys]}, Env);
negate_num_type(None = {type, _, none, []}, _Env) ->
    None;
negate_num_type(?user_type() = Ty, Env) ->
    negate_num_type(normalize(Ty, Env), Env);
negate_num_type(RangeTy, _Env) ->
    %% some kind of range type like `1..3' or `neg_integer()'
    gradualizer_int:negate_int_type(RangeTy).

-spec negate_bool_type(type()) -> type().
negate_bool_type({atom, P, true}) ->
    {atom, P, false};
negate_bool_type({atom, P, false}) ->
    {atom, P, true};
negate_bool_type(Ty) ->
    Ty.

-type list_view() :: {empty | nonempty | any | none, type(), type()}.

-spec list_view(type()) -> false | list_view().
list_view(Ty = {type, _, T, Args}) when ?is_list_type(Ty) ->
    Empty =
        case T of
            nil                          -> empty;
            list                         -> any;
            nonempty_list                -> nonempty;
            maybe_improper_list          -> any;
            nonempty_improper_list       -> nonempty;
            nonempty_maybe_improper_list -> nonempty
        end,
    Elem =
        case Args of
            []      -> type(any);
            [A | _] -> A
        end,
    Elem = ?assert_type(Elem, type()),
    Term =
        case Args of
            _ when T == nil; T == list; T == nonempty_list ->
                type(nil);
            [_, B] -> B;
            _ -> type(any)  %% Don't think we get here
        end,
    Term = ?assert_type(Term, type()),
    {Empty, Elem, Term};
list_view(_) -> false.

-spec from_list_view(list_view()) -> type().
from_list_view({_, _, {type, _, none, []}}) -> type(none);
from_list_view({empty, _, _}) -> type(nil);
from_list_view({none, _, _}) -> type(none);
from_list_view({Empty, Elem, {type, _, nil, []}}) ->
    case Empty of
        any      -> type(list, [Elem]);
        nonempty -> type(nonempty_list, [Elem])
    end;
from_list_view({Empty, Elem, Term}) ->
    case Empty of
        any      -> type(maybe_improper_list, [Elem, Term]);
        nonempty -> type(nonempty_improper_list, [Elem, Term])
    end.

%% End of subtype help functions

%% Pattern matching on types
%%
%% We sometimes need to pattern match on a type in order to get to
%% its type parameter. One example is the list type, and there are
%% cases where we need to get the type of the elements.

-spec expect_list_type(type(), allow_nil_type | dont_allow_nil_type, env()) ->
          {elem_ty,  type()}    %% There is exactly one element type
        | {elem_tys, [type()]}  %% A union can give rise to multiple elem types
        | any                   %% If we don't know the element type
        | {type_error, type()}. %% If the argument is not compatible with lists

expect_list_type({type, _, T, []}, _, _)
  when T == 'list' orelse T == 'any' orelse
       T == 'nonempty_list' orelse T == 'maybe_improper_list' ->
    any;
expect_list_type({type, _, list, [ElemTy]}, _, _) ->
    {elem_ty, ElemTy};
expect_list_type({type, _, nonempty_list, [ElemTy]}, _, _) ->
    {elem_ty, ElemTy};
expect_list_type(?top() = TermTy, _EmptyOrNot, _) ->
    {elem_ty, TermTy};
expect_list_type({type, _, maybe_improper_list, [ElemTy, _]}, _, _) ->
    {elem_ty, ElemTy};
expect_list_type({type, _, nil, []}, allow_nil_type, _) ->
    any;
expect_list_type({type, _, string, []}, _, _) ->
    {elem_ty, type(char)};
expect_list_type(Union = {type, _, union, UnionTys}, N, Env) ->
    Tys = expect_list_union(UnionTys, [], no_any, N, Env),
    case Tys of
        [] ->
            {type_error, Union};
        [Ty] ->
            {elem_ty, Ty};
        _ ->
            {elem_tys, Tys}
    end;
expect_list_type(Ty, _, _) ->
    {type_error, Ty}.

-spec rewrite_list_to_nonempty_list(type()) -> type().
rewrite_list_to_nonempty_list({type, Ann, list, [ElemTy]}) ->
    {type, Ann, nonempty_list, [ElemTy]};
rewrite_list_to_nonempty_list({type, Ann, nonempty_list, [ElemTy]}) ->
    {type, Ann, nonempty_list, [ElemTy]};
rewrite_list_to_nonempty_list({type, Ann, maybe_improper_list, [ElemTy, SentinelTy]}) ->
    {type, Ann, nonempty_improper_list, [ElemTy, SentinelTy]};
rewrite_list_to_nonempty_list({type, Ann, nonempty_improper_list, [ElemTy, SentinelTy]}) ->
    {type, Ann, nonempty_improper_list, [ElemTy, SentinelTy]};
rewrite_list_to_nonempty_list({type, _, any, _} = Ty) ->
    Ty;
rewrite_list_to_nonempty_list(?top()) ->
    top();
rewrite_list_to_nonempty_list({var, _, _} = Var) ->
    Var.

-spec expect_list_union([type()], _, _, _, env()) -> any().
expect_list_union([Ty|Tys], AccTy, Any, N, Env) ->
    case expect_list_type(normalize(Ty, Env), N, Env) of
        {type_error, _} ->
            expect_list_union(Tys, AccTy, Any, N, Env);
        any ->
            expect_list_union(Tys
                             ,[type(any) | AccTy]
                             ,any
                             ,N
                             ,Env);
        {elem_ty, NTy} ->
            expect_list_union(Tys
                             ,[NTy | AccTy]
                             ,Any
                             ,N
                             ,Env);
        {elem_tys, NTys} ->
            expect_list_union(Tys
                             ,NTys ++ AccTy
                             ,Any
                             ,N
                             ,Env)
    end;
expect_list_union([], AccTy, any, _N, _Env) ->
    [type(any) | AccTy];
expect_list_union([], AccTy, _NoAny, _N, _Env) ->
    AccTy.

-spec infer_literal_string(string(), env()) -> type().
infer_literal_string("", _Env) ->
    type(nil);
infer_literal_string(Str, Env) ->
    SortedChars = ?assert_type(lists:usort(Str), [char(), ...]),
    if length(SortedChars) =< 10 ->
            %% heuristics: if there are not more than 10 different characters
            %% list them explicitly as singleton types
            CharTypes = [{char, erl_anno:new(0), C} || C <- SortedChars],
            type(nonempty_list, [normalize(type(union, CharTypes), Env)]);
       true ->
            type(nonempty_list,
                 [type(range, [{char, erl_anno:new(0), hd(SortedChars)},
                               {char, erl_anno:new(0), lists:last(SortedChars)}])])
    end.

-spec expect_tuple_type(type(), non_neg_integer(), env()) -> R when
      R :: any
         | {elem_ty, [type()]}
         | {elem_tys, [ [type()] ]}
         | {type_error, type()}.
expect_tuple_type({type, _, any, []}, _N, _Env) ->
    any;
expect_tuple_type({type, _, tuple, any}, _N, _Env) ->
    any;
expect_tuple_type({type, _, tuple, Tys} = Ty, N, _Env) ->
    Tys = ?assert_type(Tys, [type()]),
    if
        length(Tys) == N ->
            {elem_ty, Tys};
        length(Tys) /= N ->
            {type_error, Ty}
    end;
expect_tuple_type(?top() = TermTy, N, _Env) ->
    {elem_ty, lists:duplicate(N, TermTy)};
expect_tuple_type(Union = {type, _, union, UnionTys}, N, Env) ->
    Tyss = expect_tuple_union(UnionTys, [], no_any, N, Env),
    case Tyss of
        [] ->
            {type_error, Union};
        [Tys] ->
            {elem_ty, Tys};
        _ ->
            {elem_tys, Tyss}
    end;
expect_tuple_type(?user_type() = Ty, N, Env) ->
    expect_tuple_type(normalize(Ty, Env), N, Env);
expect_tuple_type(Ty, _N, _) ->
    {type_error, Ty}.


-spec expect_tuple_union(Tys, [Tys], any | no_any, non_neg_integer(), env()) -> [Tys] when
      Tys :: [type()].
expect_tuple_union([Ty|Tys], AccTy, Any, N, Env) ->
    case expect_tuple_type(Ty, N, Env) of
        {type_error, _} ->
            expect_tuple_union(Tys, AccTy, Any, N, Env);
        any ->
            expect_tuple_union(Tys, AccTy, any, N, Env);
        {elem_ty, TTy} ->
            expect_tuple_union(Tys, [TTy | AccTy], Any, N, Env);
        {elem_tys, TTys} ->
            expect_tuple_union(Tys, TTys ++ AccTy, Any, N, Env)
    end;
expect_tuple_union([], AccTy, any, N, _Env) ->
    [ lists:duplicate(N, type(any)) | AccTy];
expect_tuple_union([], AccTy, _NoAny, _N, _Env) ->
    AccTy.


-spec expect_binary_type(type(), env()) -> R when
      R :: any
         | {elem_ty, type()}
         | {elem_tys, [type()]}
         | {type_error, type()}.
expect_binary_type(?type(any), _) ->
    any;
expect_binary_type(ElemTy = {type, _, binary, _}, _) ->
    {elem_ty, ElemTy};
expect_binary_type(Union = {type, _, union, UnionTys}, Env) ->
    case expect_binary_union(UnionTys, [], Env) of
        [] ->
            {type_error, Union};
        [Ty] ->
            {elem_ty, Ty};
        Tys ->
            {elem_tys, Tys}
    end;
expect_binary_type(Ty, _) ->
    {type_error, Ty}.

-spec expect_binary_union([type()], [type()], env()) -> [type()].
expect_binary_union([Ty|Tys], AccTy, Env) ->
    case expect_binary_type(normalize(Ty, Env), Env) of
        {type_error, _} ->
            expect_binary_union(Tys, AccTy, Env);
        any ->
            expect_binary_union(Tys, [type(any) | AccTy], Env);
        {elem_ty, NTy} ->
            expect_binary_union(Tys, [NTy | AccTy], Env);
        {elem_tys, NTys} ->
            expect_binary_union(Tys, NTys ++ AccTy, Env)
    end;
expect_binary_union([], AccTy, _Env) ->
    AccTy.


-spec allow_empty_list(type()) -> type().
allow_empty_list({type, P, nonempty_list, []}) ->
    {type, P, list, []};
allow_empty_list({type, P, nonempty_list, [T]}) ->
    {type, P, list, [T]};
allow_empty_list(Ty) ->
    case list_view(Ty) of
        {_Empty, _Elem, Term} ->
            type(union, [Ty, Term]);
        false -> Ty
    end.

-type fun_ty() :: fun_ty_simple()
                | fun_ty_intersection()
                | fun_ty_union().

-type fun_ty_simple()       :: {fun_ty, [type()], type()}.
-type fun_ty_intersection() :: {fun_ty_intersection, [fun_ty_simple()]}.
-type fun_ty_union()        :: {fun_ty_union, [fun_ty()]}.

%% Categorizes a function type.
%% Normalizes the type (expand user-def and remote types). Errors for non-fun
%% types are returned with the original non-normalized type.
%% TODO: move tenv to back
-spec expect_fun_type(env(), type() | [type()], arity()) -> fun_ty() | {type_error, type()}.
expect_fun_type(Env, Type, Arity) ->
    Normalized = case Type of
                     Types when is_list(Types) ->
                         [ normalize(Ty, Env) || Ty <- Types ];
                     Type ->
                         normalize(Type, Env)
                 end,
    case expect_fun_type1(Env, Normalized, Arity) of
        type_error -> {type_error, Type};
        Other -> Other
    end.

%% TODO: move tenv to back
-spec expect_fun_type1(env(), type() | [type()], arity()) -> fun_ty() | type_error.
expect_fun_type1(Env, BTy = {type, _, bounded_fun, [Ft, _Fc]}, Arity) ->
    Sub = bounded_type_subst(Env, BTy),
    Ft = ?assert_type(Ft, type()),
    {fun_ty, ArgsTy, ResTy} = expect_fun_type1(Env, Ft, Arity),
    ArgsTy1 = subst_ty(Sub, ArgsTy),
    ResTy1 = subst_ty(Sub, ResTy),
    {fun_ty, ArgsTy1, ResTy1};
expect_fun_type1(_Env, {type, _, 'fun', [{type, _, product, ArgsTy}, ResTy]}, _Arity) ->
    {fun_ty, ArgsTy, ?assert_type(ResTy, type())};
expect_fun_type1(_Env, {type, _, 'fun', []}, Arity) ->
    ArgsTy = lists:duplicate(Arity, type(any)),
    ResTy = type(any),
    {fun_ty, ArgsTy, ResTy};
expect_fun_type1(_Env, {type, _, 'fun', [{type, _, any}, ResTy]}, Arity) ->
    ArgsTy = lists:duplicate(Arity, type(any)),
    %% We can assert the below,
    %% as we know Res2 is not {type, _, any}, which is explicitely matched on above.
    ResTy = ?assert_type(ResTy, type()),
    {fun_ty, ArgsTy, ResTy};
expect_fun_type1(Env, Tys, Arity) when is_list(Tys) ->
    %% This is a spec, not really a type().
    case expect_intersection_type(Env, Tys, Arity) of
        type_error ->
            type_error;
        [Ty] ->
            Ty;
        Tyss ->
            {fun_ty_intersection, Tyss}
    end;
expect_fun_type1(Env, {type, _, union, UnionTys}, Arity) ->
    case expect_fun_type_union(Env, UnionTys, Arity) of
        [] ->
            type_error;
        [Ty] ->
            Ty;
        Tys ->
            {fun_ty_union, Tys}
    end;
expect_fun_type1(_Env, {type, _, any, []}, Arity) ->
    ArgsTy = lists:duplicate(Arity, type(any)),
    ResTy = type(any),
    {fun_ty, ArgsTy, ResTy};
expect_fun_type1(_Env, ?top(), Arity) ->
    ArgsTy = lists:duplicate(Arity, type(any)),
    {fun_ty, ArgsTy, top()};
expect_fun_type1(_Env, _Ty, _Arity) ->
    type_error.

-spec expect_intersection_type(env(), [tuple()], arity()) -> [fun_ty_simple()] | type_error.
expect_intersection_type(_Env, [], _Arity) ->
    [];
expect_intersection_type(Env, [FunTy|Tys], Arity) ->
    case expect_fun_type1(Env, FunTy, Arity) of
        type_error ->
            type_error;
        {fun_ty_intersection, _} ->
            %% We can't have a multi-clause spec clause within a multi-clause spec
            type_error;
        {fun_ty_union, _} ->
            %% We can't have a union of functions as a spec clause
            type_error;
        Ty ->
            Ty = ?assert_type(Ty, fun_ty_simple()),
            case expect_intersection_type(Env, Tys, Arity) of
                type_error ->
                    type_error;
                Tyss ->
                    [Ty | Tyss]
            end
    end.

-spec expect_fun_type_union(env(), [tuple()], arity()) -> [fun_ty()].
expect_fun_type_union(_Env, [], _Arity) ->
    [];
expect_fun_type_union(Env, [Ty|Tys], Arity) ->
    case expect_fun_type1(Env, Ty, Arity) of
        type_error ->
            expect_fun_type_union(Env, Tys, Arity);
        TyOut ->
            [TyOut | expect_fun_type_union(Env, Tys, Arity)]
    end.

-spec expect_record_type(type(), atom(), env()) -> Any | FieldsTy | TypeError when
    Any :: any,
    FieldsTy :: {fields_ty, [typed_record_field()]},
    TypeError :: {type_error, atom() | type()}.
expect_record_type({type, _, record, [{atom, _, Name}|RefinedTypes]}, Record, Env) when Name =:= Record ->
    #env{tenv = #{records := REnv}} = Env,
    case REnv of
        #{Record := Fields} ->
            Tys =
                case RefinedTypes of
                    [] -> Fields;
                    _ ->
                        %% `RefinedTypes' and `Fields' might be of different sizes
                        RefinedMap = maps:from_list([ {FieldName, Ty} || ?type_field_type(FieldName, Ty) <- RefinedTypes ]),
                        [ {typed_record_field, RecordField, maps:get(FieldName, RefinedMap, Ty)}
                          || ?typed_record_field(FieldName) = {typed_record_field, RecordField, Ty} <- Fields ]
                end,
            {fields_ty, Tys};
        _NotFound ->
            {type_error, Record}
    end;
expect_record_type(?top() = _TermTy, _Record, _Env) ->
    any;
expect_record_type(?user_type() = Ty, Record, Env) ->
    expect_record_type(normalize(Ty, Env), Record, Env);
expect_record_type(Union = {type, _, union, UnionTys}, Record, Env) ->
    case expect_record_union(UnionTys, [], Record, Env) of
        [] ->
            {type_error, Union};
        [Tys] ->
            {fields_ty, Tys}
    end;
expect_record_type({type, _, any, []}, _Record, _Env) ->
    any;
expect_record_type(_, Ty, _) ->
    {type_error, Ty}.

-spec expect_record_union(Tys, FTyss, atom(), env()) -> FTyss when
      Tys :: [type()],
      FTyss :: [[typed_record_field()]].
expect_record_union([Ty | Tys], AccTy, Record, Env) ->
    case expect_record_type(Ty, Record, Env) of
        {type_error, _} ->
            expect_record_union(Tys, AccTy, Record, Env);
        any ->
            expect_record_union(Tys, AccTy, Record, Env);
        {fields_ty, FTys} ->
            expect_record_union(Tys, [FTys | AccTy], Record, Env)
    end;
expect_record_union([], AccTy, _Record, _Env) ->
    AccTy.

%% TODO: move tenv to back
-spec bounded_type_list_to_type(env(), [type()]) -> type().
bounded_type_list_to_type(Env, Types) ->
    case unfold_bounded_type_list(Env, Types) of
        [Ty] -> Ty;
        Tys ->
            Tys = ?assert_type(Tys, [type(),...]),
            merge_intersection_fun_tys(Tys, Env)
    end.

%% Makes a single function type out of multi-clause function types.
%% Performs union over parameter types and union over result types.
%% E.g. [fun((a) -> b), fun((c) -> d)] results in fun((a | c) -> b | d).
%% A better way would be to use intersection types, but we don't have those.
-spec merge_intersection_fun_tys([type(),...], env()) -> type().
merge_intersection_fun_tys(FunTys, Env) ->
    [FirstFunTy | RestFunTys] = FunTys,
    ?type('fun', [?type(product, InitAccArgTys), InitAccResTy]) = FirstFunTy,
    InitAcc = {InitAccArgTys, InitAccResTy},
    CombineTys = fun (Ty1, Ty2) -> lub([Ty1, Ty2], Env) end,
    MergeFunTy = fun
        (?type('fun', [?type(product, ArgTys), ResTy]), {AccArgTys, AccResTy}) ->
            NewArgTys = lists:zipwith(CombineTys, AccArgTys, ArgTys),
            NewResTy = CombineTys(AccResTy, ResTy),
            {NewArgTys, NewResTy}
    end,
    {ArgTys, ResTy} = lists:foldl(MergeFunTy, InitAcc, RestFunTys),
    %% type() | ... is currently not a subtype of type(),
    %% see test/known_problems/should_pass/different_normalization_levels.erl
    ResTy = ?assert_type(ResTy, type()),
    type_fun(ArgTys, ResTy).

%% TODO: move tenv to back
-spec unfold_bounded_type_list(env(), [type()]) -> [type()].
unfold_bounded_type_list(Env, Types) when is_list(Types) ->
    [ unfold_bounded_type(Env, Ty) || Ty <- Types ].

%% Unfolds constrained type variables in a bounded fun type. Unconstrained type
%% variables are left unchanged. Constraints on the form T :: any(), T :: term()
%% and T :: gradualizer:top() are ignored. A "plain" fun type is returned.
%% TODO: move tenv to back
-spec unfold_bounded_type(env(), type()) -> any().
unfold_bounded_type(Env, BTy = {type, _, bounded_fun, [Ty, _]}) ->
    Sub = bounded_type_subst(Env, BTy),
    subst_ty(Sub, ?assert_type(Ty, type()));
unfold_bounded_type(_Env, Ty) -> Ty.

%% TODO: move tenv to back
-spec bounded_type_subst(env(), {type, erl_anno:anno(), bounded_fun, [_]}) ->
        #{ atom() := type() }.
bounded_type_subst(Env, BTy = {type, P, bounded_fun, [_, Bounds]}) ->
    try
        solve_bounds(Env, Bounds)
    catch throw:{cyclic_dependencies, Xs} ->
        throw(type_error(cyclic_type_vars, P, BTy, Xs))
    end.

%% TODO: move tenv to back
-spec solve_bounds(env(), [gradualizer_type:af_constraint()]) -> #{ atom() := type() }.
solve_bounds(Env, Cs) ->
    Defs = [ {X, T} || {type, _, constraint, [{atom, _, is_subtype}, [{var, _, X}, T]]} <- Cs ],
    DefEnv  = lists:foldl(fun
                              ({_, ?type(any)}, E) ->
                                  E; % Ignore constraint X :: any()
                              ({_, ?type(term)}, E) ->
                                  E; % Ignore constraint X :: term()
                              ({_, ?top()}, E) ->
                                  E; % Ignore constraint X :: gradualizer:top()
                              ({X, T}, E) ->
                                  maps:update_with(X,
                                                   fun(Ts) -> [T | Ts] end,
                                                   [T],
                                                   E)
                          end, #{}, Defs),
    DepG = maps:map(fun(_, T) -> maps:keys(free_vars(T)) end, DefEnv),
    SCCs = gradualizer_lib:top_sort(DepG),
    solve_bounds(Env, DefEnv, SCCs, #{}).

%% TODO: move tenv to back
-spec solve_bounds(env(), _, _, _) -> #{ atom() := type() }.
solve_bounds(Env, Defs, [{acyclic, X} | SCCs], Acc) ->
    Ty1 =
        case Defs of
            #{X := Tys} ->
                %% Tys is a list, so Tys1 is a list, too
                %% TODO: but cannot it be a [fun_ty()], too?
                Tys1 = ?assert_type(subst_ty(Acc, Tys), [type()]),
                %% Take intersection after substitution to
                %% get rid of type variables.
                lists:foldl(fun(S, T) -> glb(S, T, Env) end, top(), Tys1);
            _NoBoundsForX ->
                %% Unconstrained type variables are kept as type variables.
                {var, erl_anno:new(0), X}
        end,
    solve_bounds(Env, maps:remove(X, Defs), SCCs, Acc#{ X => Ty1 });
solve_bounds(_, _, [{cyclic, Xs} | _], _) ->
    throw({cyclic_dependencies, Xs});
solve_bounds(_, _, [], Acc) -> Acc.

-spec free_vars(type()) -> #{atom() => true}.
free_vars(Ty) -> free_vars(Ty, #{}).

-spec free_vars([type()] | type() | any(), #{atom() => true}) -> #{atom() => true}.
free_vars({var, _, '_'}, Vars) ->
    Vars;
free_vars({var, _, X}, Vars) ->
    Vars#{ X => true };
free_vars([H | T], Vars) ->
    free_vars(T, free_vars(H, Vars));
free_vars({type, _, _, Args}, Vars) ->
    free_vars(Args, Vars);
free_vars(_, Vars) -> Vars.

-spec subst_ty(#{atom() => type()}, [type()]) -> [type()];
              (#{atom() => type()}, type()) -> type().
subst_ty(Sub, Tys) when is_list(Tys) ->
    [ subst_ty(Sub, Ty) || Ty <- Tys ];
subst_ty(Sub, Ty = {var, _, X}) ->
    maps:get(X, Sub, Ty);
subst_ty(Sub, Ty = {rigid_var, _, X}) ->
    maps:get(X, Sub, Ty);
subst_ty(Sub, {type, P, Name, Args}) ->
    {type, P, Name, subst_ty(Sub, Args)};
subst_ty(Sub, {user_type, P, Name, Args}) ->
    {user_type, P, Name, subst_ty(Sub, Args)};
subst_ty(Sub, {remote_type, P, [Mod, Fun, Args]}) ->
    {remote_type, P, [Mod, Fun, subst_ty(Sub, Args)]};
subst_ty(Sub, {ann_type, P, [AnnoVar, Type]}) ->
    {ann_type, P, [AnnoVar, subst_ty(Sub, Type)]};
subst_ty(_, Ty) -> Ty.

-type variance() :: covariant | contravariant | invariant.
-type variance_map() :: #{atom() => variance()}.

%% Gathers all flexible type variables in a given type together with their variance
%% with respect to the given type.
%%
%% Suppose R is a type that contains a type variable X.
%% R is covariant in X if for every S and T such that S <: T, it holds that [S/X]R <: [T/X]R.
%% R is contravariant in X if for every S and T such that S <: T, it holds that [T/X]R <: [S/X]R.
%% R is invariant in X if it is neither covariant nor contravariant in X.
%%
%% In Erlang all type constructors (lists, pairs, etc.) are covariant in their parameters,
%% only function types are contravariant in their parameter types and covariant in their
%% result type.
-spec type_vars_variances(type()) -> variance_map().
type_vars_variances({var, _, '_'}) ->
    #{};
type_vars_variances({var, _, Var}) ->
    #{Var => covariant};
type_vars_variances({type, _, 'fun', [{type, _, any}, ResTy]}) ->
    type_vars_variances(?assert_type(ResTy, type()));
type_vars_variances({type, _, 'fun', [{type, _, product, ArgTys}, ResTy]}) ->
    ArgsVarMaps = [ reverse_variance_map(type_vars_variances(ArgTy)) || ArgTy <- ArgTys ],
    ResVarMap = type_vars_variances(?assert_type(ResTy, type())),
    combine_variance_maps([ResVarMap | ArgsVarMaps]);
type_vars_variances({type, _, _Name, Args}) when is_list(Args) ->
    VarMaps = [ type_vars_variances(Arg) || Arg <- Args ],
    combine_variance_maps(VarMaps);
type_vars_variances({user_type, _, _Name, Args}) when is_list(Args) ->
    %% TODO: normalize to expand user types
    %% We now treat all type variables used as arguments to parametrized user types
    %% as covariant. Otherwise we would have to carry the Env and Seen only to find
    %% out that most of the occurrences would still be covariant.
    VarMaps = [ type_vars_variances(Arg) || Arg <- Args ],
    combine_variance_maps(VarMaps);
type_vars_variances(_) -> #{}.

-spec combine_variance_maps([variance_map()]) -> variance_map().
combine_variance_maps(VarMaps) ->
    MergeVariance = fun
        (X, X) -> X;
        (_, invariant) -> invariant;
        (invariant, _) -> invariant;
        (covariant, contravariant) -> invariant;
        (contravariant, covariant) -> invariant
    end,
    Reduce = fun (New, Acc) ->
        gradualizer_lib:merge_with(fun (_K, V1, V2) -> MergeVariance(V1, V2) end, Acc, New)
    end,
    lists:foldl(Reduce, #{}, VarMaps).

-spec reverse_variance_map(variance_map()) -> variance_map().
reverse_variance_map(VarMap) ->
    ReverseVariance = fun
        (covariant) -> contravariant;
        (contravariant) -> covariant;
        (invariant) -> invariant
    end,
    maps:map(fun (_K, Variance) -> ReverseVariance(Variance) end, VarMap).

%% Returns all *flexible* type variables appearing in a given type.
-spec collect_type_variables(type()) -> [atom()].
collect_type_variables(Type) ->
    Reduce = fun
        ({var, _, '_'} = Ty, Acc) -> {Ty, Acc};
        ({var, _, Var} = Ty, Acc) -> {Ty, [Var | Acc]};
        (Ty, Acc) -> {Ty, Acc}
    end,
    {_, TyVars} = typelib:reduce_type(Reduce, [], Type),
    TyVars.

-spec contains_type_variables(type()) -> boolean().
contains_type_variables(Type) ->
    collect_type_variables(Type) /= [].

-spec replace_type_vars_with_any(type()) -> type();
                                ([type()]) -> [type()].
replace_type_vars_with_any(Tys) when is_list(Tys) ->
    [replace_type_vars_with_any(?assert_type(Ty, type())) || Ty <- Tys];
replace_type_vars_with_any(Ty) ->
    TyVars = collect_type_variables(Ty),
    Subst = maps:from_list(lists:map(fun (Var) -> {Var, type(any)} end, TyVars)),
    subst_ty(Subst, Ty).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Infer type of expression
%%
%% Type information should* stem from user type annotation (function specs
%% and type annotated record definitions). If an expression does not have a
%% subexpression that has a type inferred from these sources, its inferred type
%% will be `any()'.
%%
%%   *) When the option 'infer' is true, the types of literals and other
%%      constructs are also propagated.
%%
%% Arguments: An environment for functions, an environment for variables
%% and the expression to type check.
%% Returns the type of the expression, a collection of variables bound in
%% the expression together with their type.
-spec type_check_expr(env(), expr()) -> {type(), env()}.
type_check_expr(Env, Expr) ->
    Res = {Ty, _VarBinds} = do_type_check_expr(Env, Expr),
    ?verbose(Env, "~sPropagated type of ~ts :: ~ts~n",
             [gradualizer_fmt:format_location(Expr, brief), erl_prettypr:format(Expr), typelib:pp_type(Ty)]),
    Res.

%% TODO: move tenv to back
-spec do_type_check_expr(env(), expr()) -> {type(), env()}.
do_type_check_expr(Env, {var, _P, Var}) ->
    case Env#env.venv of
        #{Var := Ty} ->
            {Ty, Env}
    end;
do_type_check_expr(Env, {match, _, Pat, Expr}) ->
    {Ty, VarBinds} = type_check_expr(Env, Expr),
    NormTy = normalize(Ty, Env),
    NewEnv = union_var_binds(VarBinds, Env, Env),
    {[_PatTy], [UBoundNorm], Env2} =
            ?throw_orig_type(add_types_pats([Pat], [NormTy], NewEnv, capture_vars), Ty, NormTy),
    UBound = case UBoundNorm of NormTy -> Ty;
                                _Other -> UBoundNorm end,
    {UBound, Env2};
do_type_check_expr(Env, {'if', _, Clauses}) ->
    infer_clauses(Env, Clauses);
do_type_check_expr(Env, {'case', _, Expr, Clauses}) ->
    {_ExprTy, Env1} = type_check_expr(Env, Expr),
    Env2 = add_var_binds(Env, Env1, Env),
    {Ty, VB} = infer_clauses(Env2, Clauses),
    {Ty, union_var_binds(Env1, VB, Env)};
do_type_check_expr(Env, {tuple, _, TS}) ->
    {Tys, VarBindsList} = lists:unzip([ type_check_expr(Env, Expr) || Expr <- TS ]),
    InferredTy = type(tuple, Tys),
    {InferredTy, union_var_binds(VarBindsList, Env)};
do_type_check_expr(Env, {cons, _, Head, Tail}) ->
    {Ty1, VB1} = type_check_expr(Env, Head),
    {Ty2, VB2} = type_check_expr(Env, Tail),
    VB = union_var_binds(VB1, VB2, Env),
    case {Ty1, Ty2} of
        {_, ?type(any)} ->
            %% Propagate type information from head
            {type(nonempty_list, [Ty1]), VB};
        {_, _} ->
            %% Propagate type information from tail, which must be a list type
            TailElemTys =
                case expect_list_type(Ty2, dont_allow_nil_type, Env) of
                    any ->
                        [type(any)];
                    {elem_ty, ElemTy} ->
                        [ElemTy];
                    {elem_tys, ElemTys} ->
                        ElemTys;
                    {type_error, ?type(nil)} ->
                        [];
                    {type_error, BadTy} ->
                        throw(type_error(list, element(2, Tail), BadTy))
                        %% We throw a type error here because Tail is not of type list
                        %% (nor is it of type any()).
                        %% TODO: Improper list?
                end,
            FinalElemTy = normalize(type(union, [Ty1|TailElemTys]), Env),
            {type(nonempty_list, [FinalElemTy]), VB}
    end;
do_type_check_expr(Env, {bin, _, BinElements} = BinExpr) ->
    %% <<Expr:Size/TypeSpecifierList, ...>>
    VarBinds =
        lists:map(fun ({bin_element, _P, Expr, _Size, _Specif} = BinElem) ->
                          %% Treat bin type specifier as type annotation
                          Ty = type_of_bin_element(BinElem, expr),
                          type_check_expr_in(Env, Ty, Expr)
                  end,
                  BinElements),
    %% Infer the size parameters of the bitstring
    RetTy = gradualizer_bin:compute_type(BinExpr),
    {RetTy, union_var_binds(VarBinds, Env)};
do_type_check_expr(Env, {call, _, {atom, _, TypeOp}, [Expr, {string, _, TypeStr} = TypeLit]})
  when TypeOp == '::'; TypeOp == ':::' ->
    %% Magic functions used as type annotation/assertion.
    try typelib:remove_pos(typelib:parse_type(TypeStr)) of
        Type when TypeOp == '::' ->
            VarBinds = type_check_expr_in(Env, Type, Expr),
            {Type, VarBinds};
        Type when TypeOp == ':::' ->
            {InferredType, VarBinds} = type_check_expr(Env, Expr),
            case compatible(InferredType, Type, Env) of
                true ->
                   {Type, VarBinds};
                false ->
                    %% TODO: Error message improvement: Expected type
                    %% should be any type compatible with Type, rather
                    %% than just Type (the 3rd tuple element).
                    throw(type_error(Expr, Type, InferredType))
            end
    catch error:_ ->
        throw(bad_type_annotation(TypeLit))
    end;
do_type_check_expr(Env, {call, _, {atom, _, record_info}, [_, _]} = Call) ->
    Ty = get_record_info_type(Call, Env),
    {Ty, Env};
do_type_check_expr(Env, {call, P, {remote, _, _Mod, {atom, _, module_info}} = Name, Args})
        when length(Args) == 0 orelse length(Args) == 1 ->
    Arity = arity(length(Args)),
    FunTy = get_module_info_type(Arity),
    type_check_call_ty(Env, expect_fun_type(Env, FunTy, Arity), Args, {Name, P, FunTy});
do_type_check_expr(Env, {call, P, Name, Args}) ->
    Name = ?assert_type(Name, expr()),
    Arity = arity(length(Args)),
    {FunTy, VarBinds1} = type_check_fun(Env, Name, Arity),
    {ResTy, VarBinds2} = type_check_call_ty(Env, expect_fun_type(Env, FunTy, Arity),
                                                 Args, {Name, P, FunTy}),
    {ResTy, union_var_binds(VarBinds1, VarBinds2, Env)};

do_type_check_expr(Env, {lc, _, Expr, Qualifiers}) ->
    type_check_comprehension(Env, lc, Expr, Qualifiers);
do_type_check_expr(Env, {bc, _, Expr, Qualifiers}) ->
    type_check_comprehension(Env, bc, Expr, Qualifiers);
do_type_check_expr(Env, {block, _, Block}) ->
    type_check_block(Env, Block);

do_type_check_expr(Env, {string, _, Chars}) ->
    ActualTy = infer_literal_string(Chars, Env),
    {ActualTy, Env};
do_type_check_expr(Env, {nil, _}) ->
    {type(nil), Env};
do_type_check_expr(Env, {Tag, _, Value})
  when Tag =:= atom;
       Tag =:= integer;
       Tag =:= char ->
    {singleton(Tag, Value), Env};
do_type_check_expr(Env, {float, _, _F}) ->
    {type(float), Env};

%% Maps
do_type_check_expr(Env, {map, _, Assocs}) ->
    {AssocTys, VB} = type_check_assocs(Env, Assocs),
    MapTy = update_map_type(type(map, []), AssocTys),
    {MapTy, VB};
do_type_check_expr(Env, {map, _, UpdateExpr, Assocs}) ->
    {Ty, VBExpr} = type_check_expr(Env, UpdateExpr),
    {AssocTys, VBAssocs} = type_check_assocs(Env, Assocs),
    MapTy = update_map_type(Ty, AssocTys),
    % TODO: Check the type of the map.
    {MapTy, union_var_binds(VBExpr, VBAssocs, Env)};

%% Records
do_type_check_expr(Env, {record_field, Anno, Expr, Record, FieldWithAnno}) ->
    {Ty, VB1} = type_check_expr(Env, Expr),
    case Ty of
        {type, _, record, [{atom, _, Record}]} ->
            % Unrefined
            VB2 = type_check_expr_in(Env, {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]}, Expr),
            Rec = get_record_fields(Record, Anno, Env),
            FieldTy = get_rec_field_type(FieldWithAnno, Rec),
            VB = union_var_binds(VB1, VB2, Env),
            {FieldTy, VB};
        {type, _, record, [{atom, _, Record} | Fields]} ->
            Rec = get_record_fields(Record, Anno, Env),
            FieldTypes1 = record_field_types(Fields),
            FieldTypes2 = record_field_types(Rec),
            FieldTy = get_rec_field_type(FieldWithAnno, FieldTypes1, FieldTypes2),
            {FieldTy, VB1};
        _ ->
            VB2 = type_check_expr_in(Env, {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]}, Expr),
            Rec = get_record_fields(Record, Anno, Env),
            FieldTy = get_rec_field_type(FieldWithAnno, Rec),
            VB = union_var_binds(VB1, VB2, Env),
            {FieldTy, VB}
    end;
do_type_check_expr(Env, {record, Anno, Expr, Record, Fields}) ->
    RecTy = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    VB1 = type_check_expr_in(Env, RecTy, Expr),
    Rec = get_record_fields(Record, Anno, Env),
    VB2 = type_check_fields_for_update(Env, Rec, Fields),
    {RecTy, union_var_binds(VB1, VB2, Env)};
do_type_check_expr(Env, {record, Anno, Record, Fields}) ->
    RecTy    = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    Rec      = get_record_fields(Record, Anno, Env),
    VB = type_check_fields(Env, Rec, Fields),
    {RecTy, VB};
do_type_check_expr(Env, {record_index, Anno, Name, FieldWithAnno}) ->
    RecFields = get_record_fields(Name, Anno, Env),
    Index = get_rec_field_index(FieldWithAnno, RecFields),
    {{integer, erl_anno:new(0), Index}, Env};

%% Functions
do_type_check_expr(Env, {'fun', _, {clauses, Clauses}}) ->
    type_check_fun(Env, Clauses);
do_type_check_expr(Env, {'fun', P, {function, Name, Arity}}) ->
    case get_bounded_fun_type_list(Name, Arity, Env, P) of
        [?type(any)] ->
            {type(any), Env};
        BoundedFunTypeList ->
            Ty = bounded_type_list_to_type(Env, BoundedFunTypeList),
            Ty1 = replace_type_vars_with_any(Ty),
            {Ty1, Env}
    end;
do_type_check_expr(Env, {'fun', P, {function, M, F, A}}) ->
    case {get_atom(Env, M), get_atom(Env, F), A} of
        {{atom, _, Module}, {atom, _, Function}, {integer, _, Arity}} ->
            Arity = arity(Arity),
            case gradualizer_db:get_spec(Module, Function, Arity) of
                {ok, BoundedFunTypeList} ->
                    Ty = bounded_type_list_to_type(Env, BoundedFunTypeList),
                    Ty1 = replace_type_vars_with_any(Ty),
                    {Ty1, Env};
                not_found ->
                    throw(call_undef(P, Module, Function, Arity))
            end;
        _ -> %% Not enough information to check the type of the call.
            {type(any), Env}
    end;
do_type_check_expr(Env, {named_fun, _, FunName, Clauses}) ->
    %% Pick a type for the fun itself, to be used when checking references to
    %% itself inside the fun, e.g. recursive calls.
    %% Create a fun type of the correct arity
    %% on the form fun((_,_,_) -> any()).
    [{clause, _, Params, _Guards, _Block} | _] = Clauses,
    Arity = arity(length(Params)),
    FunTy = create_fun_type(Arity, type(any)),
    NewEnv = add_var_binds(Env#env{venv = #{FunName => FunTy}}, Env, Env),
    type_check_fun(NewEnv, Clauses);

do_type_check_expr(Env, {'receive', _, Clauses}) ->
    infer_clauses(Env, Clauses);
do_type_check_expr(Env, {'receive', _, Clauses, _After, Block}) ->
    {TyClauses, VarBinds1} = infer_clauses(Env, Clauses),
    {TyBlock,   VarBinds2} = type_check_block(Env, Block),
    {normalize({type, erl_anno:new(0), union, [TyClauses, TyBlock]}, Env)
    ,union_var_binds(VarBinds1, VarBinds2, Env)};

%% Operators
do_type_check_expr(Env, {op, _, '!', Proc, Val}) ->
    % Message passing is untyped remotely so the types of Proc and Val are not
    % checked. Val is the return value of the expression.
    {_, VB1} = type_check_expr(Env, Proc),
    {TyVal, VB2} = type_check_expr(Env, Val),
    {TyVal
    ,union_var_binds(VB1, VB2, Env)};
do_type_check_expr(Env, {op, _, 'not', Arg} = Expr) ->
    {Ty, VB} = type_check_expr(Env, Arg),
    BoolTy = type(boolean),
    case subtype(Ty, BoolTy, Env) of
        true ->
            NormTy = normalize(Ty, Env),
            {negate_bool_type(NormTy), VB};
        false ->
            throw(type_error(Expr, Ty, BoolTy))
    end;
do_type_check_expr(Env, {op, _, 'bnot', Arg}) ->
    {Ty, VB} = type_check_expr(Env, Arg),
    IntTy = type(integer),
    case subtype(Ty, IntTy, Env) of
        true ->
            {type(integer), VB};
        false ->
            throw(type_error(Arg, Ty, IntTy))
    end;
do_type_check_expr(Env, {op, P, '+', Arg}) ->
    {Ty, VB} = type_check_expr(Env, Arg),
    case subtype(Ty, type(number), Env) of
        true ->
            {Ty, VB};
        false ->
            throw(type_error(non_number_argument_to_plus, P, Ty))
    end;
do_type_check_expr(Env, {op, P, '-', Arg}) ->
    {Ty, VB} = type_check_expr(Env, Arg),
    case subtype(Ty, type(number), Env) of
        true ->
            NormTy = normalize(Ty, Env),
            {negate_num_type(NormTy, Env), VB};
        false ->
            throw(type_error(non_number_argument_to_minus, P, Ty))
    end;
do_type_check_expr(Env, {op, _P, BoolOp, Arg1, Arg2}) when
      (BoolOp == 'andalso') or (BoolOp == 'and') or
      (BoolOp == 'orelse')  or (BoolOp == 'or') or (BoolOp == 'xor') ->
    type_check_logic_op(Env, BoolOp, Arg1, Arg2);
do_type_check_expr(Env, {op, P, RelOp, Arg1, Arg2}) when
      (RelOp == '=:=') or (RelOp == '==') or
      (RelOp == '=/=') or (RelOp == '/=') or
      % It's debatable whether we should allow comparison between any types
      % but right now it's allowed
      (RelOp == '>=')  or (RelOp == '=<') or
      (RelOp == '>')  or (RelOp == '<') ->
    type_check_rel_op(Env, RelOp, P, Arg1, Arg2);
do_type_check_expr(Env, {op, P, Op, Arg1, Arg2}) when
      Op == '+' orelse Op == '-' orelse Op == '*' orelse Op == '/' ->
    type_check_arith_op(Env, Op, P, Arg1, Arg2);
do_type_check_expr(Env, {op, P, Op, Arg1, Arg2}) when
      Op == 'div'  orelse Op == 'rem' orelse
      Op == 'band' orelse Op == 'bor' orelse Op == 'bxor' orelse
      Op == 'bsl'  orelse Op == 'bsr' ->
    type_check_int_op(Env, Op, P, Arg1, Arg2);
do_type_check_expr(Env, {op, _, Op, Arg1, Arg2}) when
      Op == '++' orelse Op == '--' ->
    type_check_list_op(Env, Arg1, Arg2);

%% Exception constructs
%% There is no typechecking of exceptions
do_type_check_expr(Env, {'catch', _, Arg}) ->
    type_check_expr(Env, Arg);
do_type_check_expr(Env, {'try', _, Block, CaseCs, CatchCs, AfterBlock}) ->
    {Ty,    VB} = type_check_block(Env, Block),
    Env2 = add_var_binds(VB, Env, Env),
    {TyC, _VB2} = infer_clauses(Env2, CaseCs),
    {TyS, _VB3} = infer_clauses(Env2, CatchCs),
    case AfterBlock of
        [] ->
            ok;
        _ ->
            type_check_block(Env2, AfterBlock)
    end,
    ResTys = case CaseCs of
        [] ->
            %% no `of' part:
            %% in case of no error, Block will be returned
            [Ty, TyS];
        _ ->
            %% there is an `of' part:
            %% in case of no error, result of the clauses will be returned
            [TyC, TyS]
    end,
    {normalize({type, erl_anno:new(0), union, ResTys}, Env), VB};

%% Maybe - value-based error handling expression
%% See https://www.erlang.org/eeps/eep-0049
do_type_check_expr(Env, {'maybe', _, _}) ->
    %% TODO: handle maybe expr properly
    {type(any), Env};
do_type_check_expr(Env, {'maybe', _, _, {'else', _, _}}) ->
    %% TODO: handle maybe expr properly
    {type(any), Env}.

%% Helper for type_check_expr for funs
-spec type_check_fun(env(), _) -> {type(), env()}.
type_check_fun(Env, Clauses) ->
    %% TODO: Infer the types of the parameters in each clause. A potential way
    %% to improve the inference for function arguments would be to give them a
    %% type variable as type. Once the constraint solver is in place it would
    %% improve the inference is certain situations.
    %% For example, if foo/1 takes arguments of type integer() and we're
    %% inferring the expression fun (X) -> foo(X) end then we can conclude that
    %% the type of X must be (a subtype of) integer() and we can give a more
    %% accurate type to the whole expression.
    %% TODO: Modify OTP's type syntax to allow returning an intersection type.
    {RetTy, _VB} = infer_clauses(Env, Clauses),
    %% Create a fun type with the correct arity on the form
    %% fun((any(), any(), ...) -> RetTy).
    [{clause, _, Params, _Guards, _Body} | _] = Clauses,
    Arity = arity(length(Params)),
    FunTy = create_fun_type(Arity, RetTy),
    %% Variable bindings inside the fun clauses are local inside the fun.
    {FunTy, Env}.

%% Creates a type on the form fun((_,_,_) -> RetTy) with the given arity.
-spec create_fun_type(arity(), type()) -> type().
create_fun_type(Arity, RetTy) when is_integer(Arity) ->
    ParTys = lists:duplicate(Arity, type(any)),
    type('fun', [type(product, ParTys), RetTy]).

%% TODO: move tenv to back
-spec type_check_fields_for_update(env(), _, _) -> _.
type_check_fields_for_update(Env, Rec, Fields) ->
    type_check_fields(Env, Rec, Fields, should_not_be_inspected).

%% TODO: move tenv to back
-spec type_check_fields(env(), [typed_record_field()], [record_field()]) -> _.
type_check_fields(Env, Rec, Fields) ->
    UnAssignedFields = get_unassigned_fields(Fields, Rec),
    type_check_fields(Env, Rec, Fields, UnAssignedFields).

%% type_check_fields for multiple cases, split by function head
%% 1. The field is present with a value, need to make sure that field type checks that value
%% 2. There is a _ present with a value, need to make sure all the remaining fields type checks that value
%% 3. If case 2 was present, we have type checked all the fields and we can stop
%% 4. If case 2 was absent, we need to make sure all the fields that were not type checked support:
%%      In the case of having a default value: putting the default value
%%      In the case of not having a default value: that it supports `undefined`
%% 5. If case 2 was absent, we have type checked all the unassigned fields.
%% TODO: move tenv to back
-spec type_check_fields(env(), _, _, _) -> env().
type_check_fields(Env, TypedRecFields, [{record_field, _, {atom, _, _} = FieldWithAnno, Expr} | Fields]
                 ,UnAssignedFields) ->
    FieldTy = get_rec_field_type(FieldWithAnno, TypedRecFields),
    VB1 = type_check_expr_in(Env, FieldTy, Expr),
    VB2 = type_check_fields(Env, TypedRecFields, Fields, UnAssignedFields),
    union_var_binds(VB1, VB2, Env);
type_check_fields(Env, TypedRecFields, [{record_field, _, {var, _, '_'}, Expr} | _Fields]
                 ,UnAssignedFields) ->
    type_check_fields(Env, TypedRecFields
                     ,[ {record_field, erl_anno:new(0)
                        ,{atom, erl_anno:new(0), Field}, Expr}
                        || Field <- UnAssignedFields]
                     ,should_not_be_inspected);
type_check_fields(Env, _TypedRecFields, [], should_not_be_inspected) ->
    Env;
type_check_fields(Env, TypedRecFields, [], [UnAssignedField|UnAssignedFields]) ->
    FieldTy = get_rec_field_type({atom, erl_anno:new(0), UnAssignedField}, TypedRecFields),
    FieldDefault = get_rec_field_default({atom, erl_anno:new(0), UnAssignedField}, TypedRecFields),
    VB1 = type_check_expr_in(Env, FieldTy, FieldDefault),
    VB2 = type_check_fields(Env, TypedRecFields, [], UnAssignedFields),
    union_var_binds(VB1, VB2, Env);
type_check_fields(Env, _TypedRecFields, [], []) ->
    Env.

-spec get_unassigned_fields(Fields, All) -> [atom()] when
      Fields :: [record_field() | typed_record_field()],
      All :: [typed_record_field()].
get_unassigned_fields(Fields, All) ->
    FieldNames = lists:flatmap(fun
                                   (?record_field(Field)) -> [Field];
                                   (?typed_record_field(Field)) -> [Field];
                                   %% We might run into values like this, which don't match above:
                                   %% {record_field, _, {var,{12,11},'_'}, _}
                                   (_) -> []
                               end, Fields),
    AllNames = lists:map(fun (?typed_record_field(Field)) -> Field end, All),
    AllNames -- FieldNames.

-spec type_check_logic_op(env(), _, _, _) -> {type(), env()}.
type_check_logic_op(Env, Op, Arg1, Arg2) ->
    % Bindings from the first argument are only passed along for
    % 'andalso' and 'orelse', not 'and', 'or' or 'xor'.
    UnionVarBindsSecondArg =
        fun (VB1, VB2) ->
                if (Op == 'and') or (Op == 'or') or (Op == 'xor') ->
                        VB1;
                   (Op == 'andalso') or (Op == 'orelse') ->
                        union_var_binds(VB1, VB2, Env)
                end
        end,
    {Ty1, VB1} = type_check_expr(Env, Arg1),
    case subtype(Ty1, type(boolean), Env) of
        false ->
            throw(type_error(Arg1, Ty1, type(boolean)));
        true ->
            {Ty2, VB2} = type_check_expr(UnionVarBindsSecondArg(Env, VB1), Arg2),
            % Allow any() in second argument for shortcut operators
            SndArgTy = if Op == 'andalso'; Op == 'orelse' -> type(any);
                          true                            -> type(bool) end,
            case subtype(Ty2, SndArgTy, Env) of
                false ->
                    throw(type_error(Arg2, Ty2, type(boolean)));
                true ->
                    Inferred =
                        case Op of
                            'andalso' -> type(union, [Ty2, {atom, erl_anno:new(0), false}]);
                            'orelse'  -> type(union, [Ty2, {atom, erl_anno:new(0), true}]);
                            _         -> type(boolean)
                        end,
                    {normalize(Inferred, Env)
                    ,union_var_binds(VB1, VB2, Env)}
            end
    end.

-spec type_check_rel_op(env(), _, _, _, _) -> {type(), env()}.
type_check_rel_op(Env, Op, P, Arg1, Arg2) ->
    case {type_check_expr(Env, Arg1)
         ,type_check_expr(Env, Arg2)} of
        {{Ty1, VB1}, {Ty2, VB2}} ->
            case compatible(Ty1, Ty2, Env) of
                true ->
                    RetType =
                        case {Ty1, Ty2} of
                            {{type, _, any, []},_} ->
                                type(any);
                            {_,{type, _, any, []}} ->
                                type(any);
                            {_,_} ->
                                % Return boolean() when both argument types
                                % are known, i.e. not any().
                                type(boolean)
                        end,
                    {RetType
                    ,union_var_binds(VB1, VB2, Env)};
                _ ->
                    throw(type_error(relop, Op, P, Ty1, Ty2))
            end
    end.

-spec type_check_arith_op(env(), _, _, _, _) -> {type(), env()}.
type_check_arith_op(Env, Op, P, Arg1, Arg2) ->
    {Ty1, VB1} = type_check_expr(Env, Arg1),
    {Ty2, VB2} = type_check_expr(Env, Arg2),

    case compat_arith_type(Ty1, Ty2, Env) of
        false ->
          throw(type_error(arith_error, Op, P, Ty1, Ty2));
        Ty ->
            {Ty, union_var_binds(VB1, VB2, Env)}
    end.

-spec type_check_int_op(env(), _, _, _, _) -> {type(), env()}.
type_check_int_op(Env, Op, P, Arg1, Arg2) ->
    {Ty1, VB1} = type_check_expr(Env, Arg1),
    {Ty2, VB2} = type_check_expr(Env, Arg2),

    case compat_arith_type(Ty1, Ty2, Env) of
        false ->
            throw(type_error(int_error, Op, P, Ty1, Ty2));
        {type, _, Ty, []} when Ty == float orelse Ty == number ->
            throw(type_error(int_error, Op, P, Ty1, Ty2));
        Ty ->
            {Ty, union_var_binds(VB1, VB2, Env)}
    end.

-spec type_check_list_op(env(), _, _) -> {type(), env()}.
type_check_list_op(Env, Arg1, Arg2) ->
    {Ty1, VB1} = type_check_expr(Env, Arg1),
    {Ty2, VB2} = type_check_expr(Env, Arg2),
    ListTy = type(list),
    case {subtype(Ty1, ListTy, Env), subtype(Ty2, ListTy, Env)} of
        {true, true} ->
            {normalize(type(union, [Ty1, Ty2]), Env),
             union_var_binds(VB1, VB2, Env)};
        {false, _} ->
            throw(type_error(Arg1, Ty1, type(list)));
        {_, false} ->
            throw(type_error(Arg2, Ty2, type(list)))
    end.

-spec type_check_call_ty(env(), fun_ty() | {type_error, _}, [expr()], _) -> {type(), env()}.
type_check_call_ty(Env, {fun_ty, ArgsTy, ResTy} = Ty, Args, {_, P, _} = E) ->
    case {length(ArgsTy), length(Args)} of
        {L, L} ->
            %% type() | ... is currently not a subtype of type(),
            %% see test/known_problems/should_pass/different_normalization_levels.erl
            AllTys = ?annotate_type([ResTy | ArgsTy], [type()]),
            case lists:any(fun contains_type_variables/1, AllTys) of
                false ->
                    VarBindsList =
                        [ type_check_expr_in(Env, ArgTy, Arg)
                            || {ArgTy, Arg} <- lists:zip(ArgsTy, Args)
                        ],
                    {ResTy, union_var_binds(VarBindsList, Env)};
                true ->
                    type_check_poly_call(Env, Ty, Args, E)
            end;
        {LenTy, LenArgs} ->
            throw(argument_length_mismatch(P, arity(LenTy), arity(LenArgs)))
    end;
type_check_call_ty(Env, {fun_ty_intersection, ClauseTys}, Args, E) ->
    {ResTy, VarBinds} = type_check_call_ty_intersect(Env, ClauseTys, Args, E),
    {ResTy, VarBinds};
type_check_call_ty(Env, {fun_ty_union, Tyss}, Args, E) ->
    {ResTy, VarBinds} = type_check_call_ty_union(Env, Tyss, Args, E),
    {ResTy, VarBinds};
type_check_call_ty(_Env, {type_error, _}, _Args, {Name, _P, FunTy}) ->
    throw(type_error(Name, FunTy, type('fun'))).

-spec type_check_poly_call(env(), fun_ty_simple(), [type()], {_, anno(), type()}) -> {type(), env()}.
type_check_poly_call(Env = #env{solve_constraints = false}, {fun_ty, ParamTys, ResTy}, Args, E) ->
    %% If solve_constraints = false, replace all type variables in the function signature with any(),
    %% and then proceed as with any monomorphic function call
    ParamTysAny = [ replace_type_vars_with_any(Ty) || Ty <- ParamTys],
    ResTyAny = replace_type_vars_with_any(ResTy),
    FunTyAny = {fun_ty, ParamTysAny, ResTyAny},
    type_check_call_ty(Env, FunTyAny, Args, E);
type_check_poly_call(Env, {fun_ty, ParamTys, ResTy}, Args, {Name, P, FunTy}) ->
    %% To typecheck calls to polymorphic functions, we employ a Local Type Inference approach,
    %% namely the Local Type Argument Synthesis algorithm from the Local Type Inference paper
    %% by Pierce and Turner (https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf).
    %% In essence it goes like this:
    %% 1. check that all argument types are subtypes of the expected parameter types
    %%    and gather constraints (upper and lower bounds for each type variables)
    %%    along the way
    %% 2. combine the bounds (lower bounds with LUB, upper bounds with GLB)
    %% 3. check that the constraints are satisfiable (i.e., all lower bounds are subtypes
    %%    of corresponding upper bounds), and if not, throw a type error
    %% 4. compute a substitution that minimizes the result type (and thus preserves
    %%    as much information as possible)
    %% 5. apply this substitution on the result type and return it
    %% For more details, see the paper (mainly chapter 3).
    %%
    %% We extend the original algorithm a bit by using a richer type language and
    %% in particular the gradual type any(). We treat any() as any other type, i.e.,
    %% we include it to constraints and substitutions and so on. It breaks some
    %% theoretical properties but in practice it works. We also infer any() at places
    %% where the original algorithm would get stuck.
    %%
    %% Note that this is the only place (plus related functions) where we deal with flexible
    %% type variables. Types returned from this function do not contain any type variables.
    %%
    %% For more information, see:
    %% https://github.com/josefs/Gradualizer/wiki/Polymorphism#typechecking-polymorphic-calls
    {ArgTys, VarBindsList} = lists:unzip([ type_check_expr(Env, Arg) || Arg <- Args  ]),
    Css = lists:map(fun ({ParamTy, Arg, ArgTy}) ->
            case subtype_with_constraints(ArgTy, ParamTy, Env) of
                {true, Cs} -> Cs;
                false -> throw(type_error(Arg, ArgTy, ParamTy))
            end
        end, lists:zip3(ParamTys, Args, ArgTys)),
    CombinedCs = constraints:combine(Css, Env),
    CallExpr = {call, P, Name, Args},
    ?verbose(Env, "~sConstraints: ~p~n", [gradualizer_fmt:format_location(CallExpr, brief), CombinedCs]),
    case constraints:satisfiable(CombinedCs, Env) of
        true -> ok;
        {false, Var, LBound, UBound} ->
            throw({constraint_error, Var, LBound, UBound, CallExpr, FunTy, ArgTys})
    end,
    Subst = minimal_substitution(CombinedCs, ResTy),
    ?verbose(Env, "~sSubstitution: ~p~n", [gradualizer_fmt:format_location(CallExpr, brief), Subst]),
    NewResTy = subst_ty(Subst, ?assert_type(ResTy, type())),

    NewEnv = union_var_binds(VarBindsList, Env),
    {NewResTy, NewEnv}.

-spec type_check_call_ty_intersect(env(), _, _, _) -> {type(), env()}.
type_check_call_ty_intersect(Env, ClauseTys, Args, E = {Name, P, FunTy}) ->
    check_call_arity(hd(ClauseTys), Args, E),
    ArgTypes = infer_arg_types(Args, Env),
    ArgExpandedUnions = lists:map(fun
                                      (?type(union, Tys)) -> Tys;
                                      (Ty) -> [Ty]
                                  end, ArgTypes),
    ArgTyCombinations = gradualizer_lib:cartesian_product(ArgExpandedUnions),
    MatchingClauses = [ Clause
                || {fun_ty, ClauseParamTys, _} = Clause <- ClauseTys,
                   ArgTys <- ArgTyCombinations,
                   true <- [
                             lists:foldl(fun
                                             (_, false) ->
                                                 false;
                                             ({ArgTy, ParamTy}, true) ->
                                                 ParamTy1 = replace_type_vars_with_any(ParamTy),
                                                 ParamTy1 = ?assert_type(ParamTy1, type()),
                                                 subtype(ArgTy, ParamTy1, Env)
                                         end,
                                         true,
                                         lists:zip(ArgTys, ClauseParamTys))
                            ] ],
    NMatches = length(MatchingClauses),
    NArgTyCombinations = length(ArgTyCombinations),
    if
        NMatches < NArgTyCombinations ->
            throw(type_error(call_intersect, P, Name, FunTy, ArgTypes));
        NMatches >= NArgTyCombinations ->
            ResTys = [ ResTy || {fun_ty, _, ResTy} <- MatchingClauses ],
            ResTys1 = replace_type_vars_with_any(ResTys),
            {lub(ResTys1, Env), Env}
    end.

%% Computes a minimal substitution with respect to the `ResTy' that satisfies given constraints.
-spec minimal_substitution(constraints:t(), type()) -> #{atom() => type()}.
minimal_substitution(Cs, ResTy) ->
    TyVarsVariances = type_vars_variances(ResTy),
    Fun = fun
        (Var, covariant) -> maps:get(Var, Cs#constraints.lower_bounds, type(any));
        (Var, contravariant) -> maps:get(Var, Cs#constraints.upper_bounds, type(any));
        (Var, invariant) ->
            LBound = maps:get(Var, Cs#constraints.lower_bounds, type(any)),
            UBound = maps:get(Var, Cs#constraints.upper_bounds, type(any)),
            case LBound == UBound of
                true -> LBound;
                false -> type(union, [LBound, type(any)])
            end
    end,
    Subst = maps:map(Fun, TyVarsVariances),
    %% type() | ... is currently not a subtype of type(),
    %% see test/known_problems/should_pass/different_normalization_levels.erl
    ?assert_type(Subst, #{atom() => type()}).

-spec infer_arg_types([expr()], env()) -> [type()].
infer_arg_types(Args, Env) ->
    lists:map(fun (Arg) ->
                      {ArgTy, _VB} = type_check_expr(Env, Arg),
                      ArgTy
              end, Args).

-spec type_check_call_ty_union(env(), _, _, _) -> {type(), env()}.
type_check_call_ty_union(Env, Tys, Args, E) ->
    {ResTys, VBs} =
        lists:unzip([type_check_call_ty(Env, Ty, Args, E)
                     || Ty <- Tys]),
    {normalize(type(union, ResTys), Env),
     union_var_binds(VBs, Env)}.

-spec compat_arith_type(type(), type(), env()) -> type() | false.
compat_arith_type(Any = {type, _, any, []}, {type, _, any, []}, _Env) ->
    Any;
compat_arith_type(Any = {type, _, any, []}, Ty, Env) ->
    case subtype(Ty, type(number), Env) of
        false ->
            false;
        _ ->
            Any
    end;
compat_arith_type(Ty, Any = {type, _, any, []}, Env) ->
    case subtype(Ty, type(number), Env) of
        false ->
            false;
        _ ->
            Any
    end;
compat_arith_type(Ty1, Ty2, Env) ->
    TInteger = type(integer),
    case {subtype(Ty1, TInteger, Env), subtype(Ty2, TInteger, Env)} of
        {true, true} ->
            TInteger;
        _ ->
            TFloat = type(float),
            case {subtype(Ty1, TFloat, Env), subtype(Ty2, TFloat, Env)} of
                {true, true} ->
                    TFloat;
                _ ->
                    TNumber = type(number),
                    case {subtype(Ty1, TNumber, Env), subtype(Ty2, TNumber, Env)} of
                        {true, true} ->
                            TNumber;
                        _ ->
                            false
                    end
            end
    end.

-spec type_check_comprehension(env(), _, expr(), _) -> {type(), env()}.
type_check_comprehension(Env, lc, Expr, []) ->
    {Ty, _VB} = type_check_expr(Env, Expr),
    RetTy = {type, erl_anno:new(0), list, [Ty]},
    {RetTy, Env};
type_check_comprehension(Env, bc, Expr, []) ->
    {Ty, _VB} = type_check_expr(Env, Expr),
    RetTy = case normalize(Ty, Env) of
                {type, _, any, []} = Any ->
                    Any;
                {type, _, binary, [{integer, _, 0}, {integer, _, _N}]} = BinType ->
                    %% A multiple of N bits.
                    BinType;
                {type, _, binary, [{integer, _, M}, {integer, _, 0}]} ->
                    %% A fixed size of M bits that we repeat multiple times
                    {type, erl_anno:new(0), binary,
                     [{integer, erl_anno:new(0), 0},
                      {integer, erl_anno:new(0), M}]};
                {type, _, binary, [{integer, _, _M}, {integer, _, _N}]} ->
                    %% A bitstring of some combined size
                    type(any);
                {type, _, union, _} ->
                    %% Possibly a union of bitstring types; ok for now.
                    type(any);
                NormTy ->
                    BitstringTy = {type, erl_anno:new(0), binary,
                            [{integer, erl_anno:new(0), 0},
                             {integer, erl_anno:new(0), 1}]},
                    throw(type_error(Expr, NormTy, BitstringTy))
            end,
    RetTy = ?assert_type(RetTy, type()),
    {RetTy, Env};
type_check_comprehension(Env, Compr, Expr, [{generate, _, Pat, Gen} | Quals]) ->
    {Ty,  _} = type_check_expr(Env, Gen),
    case expect_list_type(Ty, allow_nil_type, Env) of
        {elem_ty, ElemTy} ->
            {_PatTys, _UBounds, NewEnv} =
                add_types_pats([Pat], [ElemTy], Env, capture_vars),
            {TyL, VB} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
            {TyL, VB};
        any ->
            NewEnv = add_any_types_pat(Pat, Env),
            {TyL, VB} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
            {TyL, VB};
        {elem_tys, _ElemTys} ->
            %% As a hack, we treat a union type as any, just to
            %% allow the program to type check.
            %% TODO: Rewrite the union outside of the comprehension
            NewEnv = add_any_types_pat(Pat, Env),
            {TyL, VB} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
            {TyL, VB};
        {type_error, BadTy} ->
            throw(type_error(Gen, BadTy, type(list)))
    end;
type_check_comprehension(Env, Compr, Expr, [{b_generate, _P, Pat, Gen} | Quals]) ->
    BitStringTy = type(binary, [{integer, erl_anno:new(0), 0},
                                {integer, erl_anno:new(0), 1}]),
    VarBinds1 =
        type_check_expr_in(Env, BitStringTy, Gen),
    {_PatTys, _UBounds, NewEnv} =
        add_types_pats([Pat], [BitStringTy], Env, capture_vars),
    {TyL, VarBinds2} =
        type_check_comprehension(NewEnv, Compr, Expr, Quals),
    {TyL, union_var_binds(VarBinds1, VarBinds2, Env)};
type_check_comprehension(Env, Compr, Expr, [Guard | Quals]) ->
    %% We don't require guards to return a boolean.
    %% This decision is up for debate.
    {_Ty, VarBinds1} = type_check_expr(Env, Guard),
    NewEnv = add_var_binds(Env, VarBinds1, Env),
    {TyL, VarBinds2} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
    {TyL, union_var_binds(VarBinds1, VarBinds2, Env)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checking the type of an expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec type_check_expr_in(Env, ResTy, Expr) -> Env when
      Env :: env(),
      ResTy :: type(),
      Expr :: expr().
type_check_expr_in(Env, ResTy, Expr) ->
    ?verbose(Env, "~sChecking that ~ts :: ~ts~n",
            [gradualizer_fmt:format_location(Expr, brief), erl_prettypr:format(Expr), typelib:pp_type(ResTy)]),
    NormResTy = normalize(ResTy, Env),
    R = ?throw_orig_type(do_type_check_expr_in(Env, NormResTy, Expr), ResTy, NormResTy),
    ?assert_type(R, env()).

-spec do_type_check_expr_in(Env, ResTy, Expr) -> Env when
      Env :: env(),
      ResTy :: type(),
      Expr :: expr().
do_type_check_expr_in(Env, {type, _, any, []}, Expr) ->
    {_Ty, VB} = type_check_expr(Env, Expr),
    VB;
do_type_check_expr_in(Env, Ty, {var, _, Name} = Var) ->
    VarTy = maps:get(Name, Env#env.venv),
    case subtype(VarTy, Ty, Env) of
        true ->
            Env;
        false ->
            throw(type_error(Var, VarTy, Ty))
    end;
do_type_check_expr_in(Env, Ty, {match, _, Pat, Expr}) ->
    VarBinds = type_check_expr_in(Env, Ty, Expr),
    {_PatTys, _UBounds, NewEnv} =
        add_types_pats([Pat], [Ty], Env, capture_vars),
    union_var_binds(VarBinds, NewEnv, Env);
do_type_check_expr_in(Env, Ty, Singleton = {Tag, _, Value}) when Tag =:= integer;
                                                                 Tag =:= char;
                                                                 Tag =:= atom ->
    ActualTy = singleton(Tag, Value),
    case subtype(ActualTy, Ty, Env) of
        true ->
            Env;
        false ->
            throw(type_error(Singleton, ActualTy, Ty))
    end;
do_type_check_expr_in(Env, Ty, {float, _, _} = Float) ->
    ExpectedType = type(float),
    case subtype(ExpectedType, Ty, Env) of
        true ->
            Env;
        false ->
            throw(type_error(Float, ExpectedType, Ty))
    end;
do_type_check_expr_in(Env, Ty, Cons = {cons, _, H, T}) ->
    case expect_list_type(Ty, dont_allow_nil_type, Env) of
        {elem_ty, ETy} ->
            VB1 = type_check_expr_in(Env, ETy, H),
            VB2 = type_check_expr_in(Env, allow_empty_list(Ty),  T),
            union_var_binds(VB1, VB2, Env);
        {elem_tys, ETys} ->
            VB1 = type_check_union_in(Env, ETys, H),
            VB2 = type_check_expr_in (Env, allow_empty_list(Ty), T),
            union_var_binds(VB1, VB2, Env);
        any ->
            {_Ty, VB1} = type_check_expr   (Env, H),
                  VB2  = type_check_expr_in(Env, allow_empty_list(Ty), T),
            union_var_binds(VB1, VB2, Env);
        {type_error, _} ->
            throw(type_error(Cons, type(nonempty_list), Ty))
    end;
do_type_check_expr_in(Env, Ty, {nil, _} = Nil) ->
    case subtype(type(nil), Ty, Env) of
        true ->
            Env;
        false ->
            throw(type_error(Nil, type(nil), Ty))
    end;
do_type_check_expr_in(Env, Ty, {string, _, Chars} = String) ->
    ActualTy = infer_literal_string(Chars, Env),
    case subtype(ActualTy, Ty, Env) of
      true ->
        Env;
      false ->
        throw(type_error(String, ActualTy, Ty))
    end;
do_type_check_expr_in(Env, Ty, {bin, _Anno, _BinElements} = Bin) ->
    BinTy = gradualizer_bin:compute_type(Bin),
    case subtype(BinTy, Ty, Env) of
        true ->
            ok;
        false ->
            throw(type_error(Bin, BinTy, Ty))
    end,
    {_Ty, VarBinds} = type_check_expr(Env, Bin),
    VarBinds;
do_type_check_expr_in(Env, ResTy, {tuple, _, TS} = Tup) ->
    case expect_tuple_type(ResTy, length(TS), Env) of
        {elem_ty, Tys} ->
            VBs = [ type_check_expr_in(Env, Ty, Expr)
                 || {Ty, Expr} <- lists:zip(Tys, TS) ],
            union_var_binds(VBs, Env);
        {elem_tys, Tyss} ->
            case type_check_tuple_union_in(Env, Tyss, TS) of
                none ->
                    {Ty, _VB} = type_check_expr(Env, Tup),
                    throw(type_error(Tup, Ty, ResTy));
                VBs ->
                    union_var_binds(VBs, Env)
            end;
        any ->
            {_Tys, VBs} = lists:unzip([type_check_expr(Env, Expr) || Expr <- TS ]),
            union_var_binds(VBs, Env);
        {type_error, _} ->
            {Ty, _VB} = type_check_expr(Env, Tup),
            throw(type_error(Tup, Ty, ResTy))
    end;

%% Maps
do_type_check_expr_in(Env, ResTy, {map, _, Assocs} = MapCreation) ->
    {AssocTys, VBs} = type_check_assocs(Env, Assocs),
    %% We know that the exact type can be fully determined from `MapCreation' expression.
    %% We start from an empty map (`type(map, [])') and use `AssocTys' to build the complete type.
    %% The inferred type will have only non-optional associations (as the fields are already there
    %% when we build the map).
    MapTy = update_map_type(type(map, []), AssocTys),
    case subtype(MapTy, ResTy, Env) of
        true ->
            VBs;
        false ->
            throw(type_error(MapCreation, MapTy, ResTy))
    end;
do_type_check_expr_in(Env, ResTy, {map, _, Expr, Assocs} = MapUpdate) ->
    {Ty, VBExpr} = type_check_expr(Env, Expr),
    {AssocTys, VBAssocs} = type_check_assocs(Env, Assocs),
    UpdatedTy = update_map_type(Ty, AssocTys),
    case subtype(UpdatedTy, ResTy, Env) of
        true ->
            union_var_binds(VBExpr, VBAssocs, Env);
        false ->
            throw(type_error(MapUpdate, UpdatedTy, ResTy))
    end;

%% Records
do_type_check_expr_in(Env, ResTy, {record, Anno, Name, Fields} = Record) ->
    case expect_record_type(ResTy, Name, Env) of
        {fields_ty, Rec} ->
            type_check_fields(Env, Rec, Fields);
        any ->
            Rec = get_record_fields(Name, Anno, Env),
            type_check_fields(Env, Rec, Fields);
        {type_error, _} ->
            throw(type_error(Record,
                             type(record, [{atom, erl_anno:new(0), Name}]),
                             ResTy))
    end;
do_type_check_expr_in(Env, ResTy, {record, Anno, Exp, Name, Fields} = Record) ->
    case expect_record_type(ResTy, Name, Env) of
        {fields_ty, Rec} ->
            VarBindsList
                = lists:map(fun ({record_field, _, FieldWithAnno, Expr}) ->
                    FieldTy = get_rec_field_type(FieldWithAnno, Rec),
                    type_check_expr_in(Env, FieldTy, Expr)
                          end
                    ,Fields),
            RecordTy = type_record(Name, [type_field_type(FieldName, Type) || ?typed_record_field(FieldName, Type) <- Rec]),
            VarBinds = type_check_expr_in(Env, RecordTy, Exp),
            union_var_binds([VarBinds|VarBindsList], Env);
        any ->
            Rec = get_record_fields(Name, Anno, Env),
            type_check_fields(Env, Rec, Fields);
        {type_error, _} ->
            throw(type_error(Record,
                             type(record, [{atom, erl_anno:new(0), Name}]),
                             ResTy))
    end;
do_type_check_expr_in(Env, ResTy, {record_field, _Anno, _Expr, _Name, _FieldWithAnno} = RecordField) ->
    {RefinedFieldTy, VarBinds} = type_check_expr(Env, RecordField),
    case subtype(RefinedFieldTy, ResTy, Env) of
        true ->
            VarBinds;
        false ->
            throw(type_error(RecordField, RefinedFieldTy, ResTy))
    end;
do_type_check_expr_in(Env, ResTy, {record_index, Anno, Name, FieldWithAnno} = RecIndex) ->
    RecFields = get_record_fields(Name, Anno, Env),
    Index = get_rec_field_index(FieldWithAnno, RecFields),
    IndexTy = {integer, erl_anno:new(0), Index},
    case subtype(IndexTy, ResTy, Env) of
        true ->
            Env;
        false ->
            throw(type_error(RecIndex, IndexTy, ResTy))
    end;

do_type_check_expr_in(Env, ResTy, {'case', _, Expr, Clauses}) ->
    {ExprTy, VarBinds} = type_check_expr(Env, Expr),
    Env1 = add_var_binds(Env, VarBinds, Env),
    check_clauses(Env1, [ExprTy], ResTy, Clauses, capture_vars);
do_type_check_expr_in(Env, ResTy, {'if', _, Clauses}) ->
    check_clauses(Env, [], ResTy, Clauses, capture_vars);
do_type_check_expr_in(Env, ResTy,
                      {call, _, {atom, _, TypeOp},
                             [Expr, {string, _, TypeStr} = TypeLit]} = TyAnno)
  when TypeOp == '::'; TypeOp == ':::' ->
    try typelib:remove_pos(typelib:parse_type(TypeStr)) of
        Type ->
            case subtype(Type, ResTy, Env) of
                true when TypeOp == '::' ->
                    type_check_expr_in(Env, Type, Expr);
                true when TypeOp == ':::' ->
                    {InferredType, VarBinds} = type_check_expr(Env, Expr),
                    case compatible(InferredType, Type, Env) of
                        true ->
                            VarBinds;
                        false ->
                            %% TODO: Error message improvement: Expected type
                            %% should be any type compatible with Type, rather
                            %% than just Type (the 3rd tuple element).
                            throw(type_error(Expr, Type, InferredType))
                    end;
                false ->
                    throw(type_error(TyAnno, ResTy, Type))
            end
    catch error:_ ->
        throw(bad_type_annotation(TypeLit))
    end;
do_type_check_expr_in(Env, ResTy, {call, _, {atom, _, record_info}, [_, _]} = Call) ->
    Ty = get_record_info_type(Call, Env),
    case subtype(Ty, ResTy, Env) of
        true ->
            Env;
        false ->
            throw(type_error(Call, ResTy, Ty))
    end;
do_type_check_expr_in(Env, ResTy, {call, P, {remote, _, _Mod, {atom, _, module_info}} = Name, Args} = Call)
        when length(Args) == 0 orelse length(Args) == 1 ->
    Arity = arity(length(Args)),
    FunTy = get_module_info_type(Arity),
    type_check_call(Env, ResTy, Call, expect_fun_type(Env, FunTy, Arity),
                    Args, {P, Name, FunTy});
do_type_check_expr_in(Env, ResTy, {call, P, Name, Args} = OrigExpr) ->
    Name = ?assert_type(Name, expr()),
    Arity = arity(length(Args)),
    {FunTy, VarBinds} = type_check_fun(Env, Name, Arity),
    VarBinds2 = type_check_call(Env, ResTy, OrigExpr, expect_fun_type(Env, FunTy, Arity),
                                Args, {P, Name, FunTy}),
    union_var_binds(VarBinds, VarBinds2, Env);
do_type_check_expr_in(Env, ResTy, {lc, P, Expr, Qualifiers} = OrigExpr) ->
    type_check_comprehension_in(Env, ResTy, OrigExpr, lc, Expr, P, Qualifiers);
do_type_check_expr_in(Env, ResTy, {bc, P, Expr, Qualifiers} = OrigExpr) ->
    type_check_comprehension_in(Env, ResTy, OrigExpr, bc, Expr, P, Qualifiers);

%% Functions
do_type_check_expr_in(Env, Ty, {'fun', _, {clauses, Clauses}} = Fun) ->
    case expect_fun_type(Env, Ty, clause_arity(hd(Clauses))) of
        {fun_ty, ArgsTys, ResTy} ->
            check_clauses(Env, ArgsTys, ResTy, Clauses, bind_vars);
        %% TODO: Can this case actually happen?
        {fun_ty_intersection, Tyss} ->
            check_clauses_intersect(Env, Tyss, Clauses);
        {fun_ty_union, Tyss} ->
            check_clauses_union(Env, Tyss, Clauses);
        {type_error, _} ->
            throw(type_error(Fun, type('fun'), Ty))
    end;
do_type_check_expr_in(Env, ResTy, Expr = {'fun', P, {function, Name, Arity}}) ->
    case get_bounded_fun_type_list(Name, Arity, Env, P) of
        [?type(any)] ->
            FunType = create_fun_type(Arity, type(any)),
            case subtype(FunType, ResTy, Env) of
                true -> Env;
                false -> throw(type_error(Expr, FunType, ResTy))
            end;
        BoundedFunTypeList ->
            FunTypeList = unfold_bounded_type_list(Env, BoundedFunTypeList),
            FunTypeList1 = replace_type_vars_with_any(FunTypeList),
            case any_subtype(FunTypeList1, ResTy, Env) of
                true ->
                    Env;
                false ->
                    throw(type_error(Expr, FunTypeList1, ResTy))
            end
    end;
do_type_check_expr_in(Env, ResTy, Expr = {'fun', P, {function, M, F, A}}) ->
    case {get_atom(Env, M), get_atom(Env, F), A} of
        {{atom, _, Module}, {atom, _, Function}, {integer, _,Arity}} ->
            case gradualizer_db:get_spec(Module, Function, arity(Arity)) of
                {ok, BoundedFunTypeList} ->
                    FunTypeList =
                        unfold_bounded_type_list(Env, BoundedFunTypeList),
                    FunTypeList1 = replace_type_vars_with_any(FunTypeList),
                    case any_subtype(FunTypeList1, ResTy, Env) of
                        true -> Env;
                        false -> throw(type_error(Expr, FunTypeList1, ResTy))
                    end;
                not_found ->
                    throw(call_undef(P, Module, Function, arity(Arity)))
            end;
        _ -> % We don't have enough information to check the type.
            Env
    end;
do_type_check_expr_in(Env, Ty, {named_fun, _, FunName, Clauses} = Fun) ->
    Env1 = add_var_binds(Env#env{venv = #{ FunName => Ty }}, Env, Env),
    case expect_fun_type(Env, Ty, clause_arity(hd(Clauses))) of
        {fun_ty, ArgsTy, ResTy} ->
            check_clauses(Env1, ArgsTy, ResTy, Clauses, bind_vars);
        %% TODO: Can this case actually happen?
        {fun_ty_intersection, Tyss} ->
            check_clauses_intersect(Env1, Tyss, Clauses);
        {fun_ty_union, Tyss} ->
            check_clauses_union(Env, Tyss, Clauses);
        {type_error, _} ->
            throw(type_error(Fun, type('fun'), Ty))
    end;

do_type_check_expr_in(Env, ResTy, {'receive', _, Clauses}) ->
    check_clauses(Env, [type(any)], ResTy, Clauses, capture_vars);
do_type_check_expr_in(Env, ResTy, {'receive', _, Clauses, After, Block}) ->
    VEnv = Env#env.venv,
    Env1 = check_clauses(Env, [type(any)], ResTy, Clauses, capture_vars),
    Env2 = type_check_expr_in(Env1#env{venv = VEnv}, type(integer), After),
    Env3 = type_check_block_in(Env2#env{venv = VEnv}, ResTy, Block),
    union_var_binds([Env1, Env2, Env3], Env3);
do_type_check_expr_in(Env, ResTy, {op, _, '!', Arg1, Arg2}) ->
    % The first argument should be a pid.
    {_,  VarBinds1} = type_check_expr(Env, Arg1),
    VarBinds2 = type_check_expr_in(Env, ResTy, Arg2),
    union_var_binds(VarBinds1, VarBinds2, Env);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg}) when
      Op == 'not'; Op == 'bnot'; Op == '+'; Op == '-' ->
    type_check_unary_op_in(Env, ResTy, Op, P, Arg);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '+' orelse Op == '-' orelse Op == '*' orelse Op == '/' ->
    type_check_arith_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == 'div'  orelse Op == 'rem' orelse
      Op == 'band' orelse Op == 'bor' orelse Op == 'bxor' orelse
      Op == 'bsl'  orelse Op == 'bsr' ->
    type_check_int_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, _, Op, _, _} = OrigExpr) when
      Op == 'and' orelse Op == 'or' orelse Op == 'xor' orelse
      Op == 'andalso' orelse Op == 'orelse' ->
    type_check_logic_op_in(Env, ResTy, OrigExpr);
do_type_check_expr_in(Env, ResTy, {op, _, Op, _, _} = OrigExpr) when
      Op == '=:=' orelse Op == '==' orelse
      Op == '=/=' orelse Op == '/=' orelse
      Op == '>=' orelse Op == '=<' orelse
      Op == '>' orelse Op == '<' ->
    type_check_rel_op_in(Env, ResTy, OrigExpr);
do_type_check_expr_in(Env, ResTy, {op, _, Op, _, _} = OrigExpr) when
      Op == '++' orelse Op == '--' ->
    type_check_list_op_in(Env, ResTy, OrigExpr);

do_type_check_expr_in(Env, ResTy, {block, _, Block}) ->
    type_check_block_in(Env, ResTy, Block);
do_type_check_expr_in(Env, ResTy, {'catch', _, Arg}) ->
    % TODO: Should we require ResTy to also include the possibility of
    % exceptions? But exceptions can be of any type! That would mean
    % that we require ResTy to be any(), or perhaps also top().
    % But that would make exceptions and types almost incompatible!
    type_check_expr_in(Env, ResTy, Arg);
do_type_check_expr_in(Env, ResTy, {'try', _, Block, CaseCs, CatchCs, AfterBlock}) ->
    case CaseCs of
        [] ->
            %% no `of' part, Block must return ResTy
            type_check_block_in(Env, ResTy, Block);
        _ ->
            %% there is an `of' part, argument (pattern) of each clause must
            %% accept the return type of Block
            {BlockTy, _VB} = type_check_block(Env, Block),
            %% stangely enough variable bindings are not propagated from Block
            %% to CaseCs ("unsafe" compiler complaint)
            check_clauses(Env, [BlockTy], ResTy, CaseCs, capture_vars)
    end,
    check_clauses(Env, [type(any)], ResTy, CatchCs, capture_vars),
    case AfterBlock of
        [] ->
            ok;
        _ ->
            %% return value of after block is ignored
            type_check_block(Env, AfterBlock)
    end,
    %% no variable bindings are propagated from a try expression
    %% as that would be "unsafe"
    Env;

%% Maybe - value-based error handling expression
%% See https://www.erlang.org/eeps/eep-0049
do_type_check_expr_in(_Env, _ResTy, {'maybe', _, _}) ->
    %% TODO: handle maybe expr properly
    erlang:throw({skip, maybe_expr_not_supported});
do_type_check_expr_in(_Env, _ResTy, {'maybe', _, _, {'else', _, _}}) ->
    %% TODO: handle maybe expr properly
    erlang:throw({skip, maybe_expr_not_supported}).

-spec type_check_arith_op_in(env(), type(), _, _, _, _) -> env().
type_check_arith_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    type_check_arith_op_in(Env, number, ResTy, Op, P, Arg1, Arg2).

-spec type_check_int_op_in(env(), type(), _, _, _, _) -> env().
type_check_int_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    type_check_arith_op_in(Env, integer, ResTy, Op, P, Arg1, Arg2).

-spec type_check_arith_op_in(env(), atom(), type(), _, _, _, _) -> env().
type_check_arith_op_in(Env, Kind, ResTy, Op, P, Arg1, Arg2) ->
    ResTy1 = glb(type(Kind), ResTy, Env),
    case ResTy1 of
        %% TODO: allow none() if checking against none()? Not clear that
        %% this is sensible, since in that case you'd like to only require
        %% *one* of the arguments to be none(), not both.
        {type, _, none, []} ->
            Tag = if Kind == integer -> int_error;
                     true            -> arith_error end,
            throw(type_error(Tag, Op, P, ResTy));
        %% Fall back to inference mode if target is any()
        {type, _, any, []} ->
            {_, VB} = type_check_arith_op(Env, Op, P, Arg1, Arg2),
            VB;
        _ ->
            case arith_op_arg_types(Op, ResTy1) of
                { {ArgTy11, ArgTy12}, {ArgTy21, ArgTy22} } ->
                    try
                        arith_op_do_check(ArgTy11, ArgTy12, Arg1, Arg2, Env)
                    catch
                        throw:E ->
                            try
                                arith_op_do_check(ArgTy21, ArgTy22, Arg1, Arg2, Env)
                            catch
                                throw:_ -> throw(E)
                            end
                    end;
                {ArgTy1, ArgTy2} ->
                    VarBinds1 = type_check_expr_in(Env, ArgTy1, Arg1),
                    VarBinds2 = type_check_expr_in(Env, ArgTy2, Arg2),
                    union_var_binds(VarBinds1, VarBinds2, Env);
                false ->
                    throw(type_error(op_type_too_precise, Op, P, ResTy1))
            end
    end.

-spec arith_op_do_check(type(), type(), expr(), expr(), env()) -> env().
arith_op_do_check(ArgTy1, ArgTy2, Arg1, Arg2, Env) ->
    VarBinds1 = type_check_expr_in(Env, ArgTy1, Arg1),
    VarBinds2 = type_check_expr_in(Env, ArgTy2, Arg2),
    union_var_binds(VarBinds1, VarBinds2, Env).

%% What types should be pushed into the arguments if checking an operator
%% application against a given type.

%% any() is always fine
-spec arith_op_arg_types(atom(), type()) -> R when
      R :: false
         | {type(), type()}
         | { {type(), type()}, {type(), type()} }.
arith_op_arg_types(_Op, Ty = {type, _, any, []}) ->
    {Ty, Ty};

%% '/' can't produce an integer type
arith_op_arg_types('/', Ty) when ?is_int_type(Ty) ->
    false;

%% integer() is closed under all operators except '/'
arith_op_arg_types(_, Ty = {type, _, integer, []}) ->
    {Ty, Ty};

%% float() is closed under non-integer-exclusive operations and accepts
%% any number() for '/'.
%% Some precision lost: if at least one argument is a float() the other can
%% be integer().
arith_op_arg_types(Op, Ty = {type, _, float, []}) ->
    case Op of
        _ when Op == '+'; Op == '*'; Op == '-' ->
            {Ty, Ty};
        '/' -> {type(number), type(number)}
    end;

%% Singleton types are not closed under any operations
arith_op_arg_types(_, {T, _, _}) when T == integer; T == char ->
    false;

%% pos_integer() is the result of addition even if one of the operands is 0, i.e. non_neg_integer()
arith_op_arg_types('+', ?type(pos_integer)) ->
    { {type(non_neg_integer), type(pos_integer)},
      {type(pos_integer), type(non_neg_integer)} };

%% pos_integer() is closed under '*' and 'bor',
arith_op_arg_types(Op, ?type(pos_integer) = Ty) ->
    case lists:member(Op, ['*', 'bor']) of
        true -> {Ty, Ty};
        false -> false
    end;

%% Special case for integer recursion: pos_integer() - 1 :: non_neg_integer()
arith_op_arg_types('-', ?type(non_neg_integer)) ->
    {type(pos_integer), {integer, erl_anno:new(0), 1}};

%% non_neg_integer() are closed under top except '-' and '/'
arith_op_arg_types(Op, Ty = {type, _, non_neg_integer, []}) ->
    case lists:member(Op, ['+', '*', 'div', 'rem', 'band', 'bor', 'bxor']) of
        %% Shift amounts can be negative
        _ when Op == 'bsl'; Op == 'bsr' -> {Ty, type(integer)};
        true -> {Ty, Ty};
        false -> false
    end;

%% neg_integer() is only closed under '+'
arith_op_arg_types(Op, Ty = {type, _, neg_integer, []}) ->
    case Op of
        '+' -> {Ty, Ty};
        _   -> false
    end;

%% A..B: Could do more here, but better to be conservative and predictable.
%% We check
%%  - 0..2^N-1 is closed under bit operations (not bsl)
%%  - non_neg_integer() rem 0..N+1 : 0..N
%% This lets you work with types like
%%  -type word16() :: 0..65535.
arith_op_arg_types(Op, {type, _, range, _} = Ty) ->
    case gradualizer_int:int_type_to_range(Ty) of
        {0, B} when Op == 'rem' andalso is_integer(B) ->
            TyR = gradualizer_int:int_range_to_type({0, B + 1}),
            {type(non_neg_integer), TyR};
        %% bsr and div make things smaller for any non_neg/pos second argument
        {0, _} when Op == 'bsr' ->
            {Ty, type(non_neg_integer)};
        {0, _} when Op == 'div' ->
            {Ty, type(pos_integer)};
        {0, B} when is_integer(B) ->
            case is_power_of_two(B + 1) andalso lists:member(Op, ['band', 'bor', 'bxor']) of
                true -> {Ty, Ty};
                false -> false
            end;
        _ ->
            false
    end;

%% We get normalised types here, so number() is expanded to integer() | float().
arith_op_arg_types(Op, {type, _, union, Tys}) ->
    ArgTys = [ arith_op_arg_types(Op, Ty) || Ty <- Tys ],
    case [ A || A = {_, _} <- ArgTys ] of   %% filter failures
        []      -> false;
        ArgTys1 ->
            {LeftArgs, RightArgs} = lists:unzip(ArgTys1),
            {type(union, LeftArgs), type(union, RightArgs)}
    end;

%% Cases like Op = '-', Ty = neg_integer()
arith_op_arg_types(_Op, _Ty) ->
    false.

-spec type_check_logic_op_in(env(), type(), expr()) -> env().
type_check_logic_op_in(Env, ResTy, {op, _, Op, Arg1, Arg2} = OrigExpr) when Op == 'andalso'; Op == 'orelse' ->
    Bool   = type(boolean),
    Target = case Op of
                 'andalso' -> {atom, erl_anno:new(0), false};
                 'orelse'  -> {atom, erl_anno:new(0), true}
             end,
    case subtype(Target, ResTy, Env) of
        true ->
            VarBinds1 = type_check_expr_in(Env, Bool, Arg1),
            %% variable bindings are propagated from Arg1 to Arg2
            EnvArg2 = union_var_binds(Env, VarBinds1, Env),
            VarBinds2 = type_check_expr_in(EnvArg2, ResTy, Arg2),
            union_var_binds(VarBinds1, VarBinds2, Env);
        false ->
            {Arg2Ty, _VB} = type_check_expr(Env, Arg2),
            Ty = type(union, [Arg2Ty, Target]),
            throw(type_error(OrigExpr, Ty, ResTy))
    end;
type_check_logic_op_in(Env, ResTy, {op, _, _, Arg1, Arg2} = OrigExpr) ->
    Bool = type(boolean),
    case subtype(Bool, ResTy, Env) of
        true ->
          VarBinds1 = type_check_expr_in(Env, Bool, Arg1),
          VarBinds2 = type_check_expr_in(Env, Bool, Arg2),
          union_var_binds(VarBinds1, VarBinds2, Env);
        false ->
          throw(type_error(OrigExpr, Bool, ResTy))
    end.

-spec type_check_rel_op_in(env(), type(), expr()) -> env().
type_check_rel_op_in(Env, ResTy, {op, P, Op, Arg1, Arg2} = OrigExpr) ->
    Ty = type(boolean),
    case subtype(Ty, ResTy, Env) of
        true ->
          {ResTy1, VarBinds1} = type_check_expr(Env, Arg1),
          {ResTy2, VarBinds2} = type_check_expr(Env, Arg2),
          case compatible(ResTy1, ResTy2, Env) of
              true ->
                  union_var_binds(VarBinds1, VarBinds2, Env);
              false ->
                  throw(type_error(rel_error, Op, P, ResTy1, ResTy2))
          end;
        false ->
          throw(type_error(OrigExpr, Ty, ResTy))
    end.

-spec type_check_list_op_in(env(), type(), expr()) -> env().
type_check_list_op_in(Env, ResTy, {op, P, Op, Arg1, Arg2} = Expr) ->
    Target =
        %% '--' always produces a proper list, but '++' gives you an improper
        %% list if the second argument is improper.
        case Op of
            '--' -> type(list, [top()]);
            '++' -> type(maybe_improper_list, [top()
					      ,top()])
        end,
    ResTy1 = glb(Target, ResTy, Env),
    case ResTy1 of
        {type, _, none, []} ->
            % Tested by test/should_fail/list_op.erl subtract/1
            throw(type_error(Expr, type(list), ResTy));
        {type, _, any, []} ->
            {_, VB} = type_check_list_op(Env, Arg1, Arg2),
            VB;
        _ ->
            case list_op_arg_types(Op, ResTy1) of
                {ArgTy1, ArgTy2} ->
                    VarBinds1 = type_check_expr_in(Env, ArgTy1, Arg1),
                    VarBinds2 = type_check_expr_in(Env, ArgTy2, Arg2),
                    union_var_binds(VarBinds1, VarBinds2, Env);
                false ->
                    %% TODO: we're getting this because of a type var, so if we solve constraints
                    %% here and get a substitution for this type var, we should be able to tell more
                    throw(type_error(op_type_too_precise, Op, P, ResTy1))
            end
    end.

-spec list_op_arg_types(any(), type()) -> {type(), type()} | false.
list_op_arg_types(ListOp, {type, _, union, Tys}) ->
    %% TODO: This approximates union of lists with list of unions
    Pairs = [list_op_arg_types(ListOp, Ty) || Ty <- Tys],
    case lists:member(false, Pairs) of
        true ->
            %% Some type in the union is not a list type
            false;
        false ->
            %% We explicitly check if `false' is a member of `Pairs'.
            Pairs = ?assert_type(Pairs, [{type(), type()}]),
            {Arg1Tys, Arg2Tys} = lists:unzip(Pairs),
            {type(union, Arg1Tys), type(union, Arg2Tys)}
    end;
list_op_arg_types('++', Ty) ->
    case list_view(Ty) of
        false            -> false;
        {empty, _, _}    -> {type(nil), type(nil)};
        {Empty, Elem, _} ->
            Arg1 = from_list_view({Empty, Elem, type(nil)}),
            {Arg1, Ty}
    end;
list_op_arg_types('--', Ty) ->
    case list_view(Ty) of   %% Could go with [A] -- [top()] :: [A], but would miss legitimate errors
        false            -> false;
        {any, Elem, _}   -> {type(list, [Elem]), type(list, [Elem])};
        {empty, _, _}    -> {type(nil), type(list, [top()])};
        {nonempty, _, _} -> false
    end.

-spec type_check_unary_op_in(env(), type(), _, _, _) -> env().
type_check_unary_op_in(Env, ResTy, Op, P, Arg) ->
    Target =
        case Op of
            'not'  -> type(boolean);
            'bnot' -> type(integer);
            '+'    -> type(number);
            '-'    -> type(number)
        end,
    ResTy1 = glb(Target, ResTy, Env),
    case ResTy1 of
        %% TODO: allow if ResTy == none()?
        {type, _, none, []} ->
            throw(type_error(unary_error, Op, P, Target, ResTy));
        _ ->
            ArgTy = unary_op_arg_type(Op, ResTy1),
            type_check_expr_in(Env, ArgTy, Arg)
    end.

%% Which type should we check the argument against if we want the given type
%% out? We already know that Ty is a subtype of the return type of the operator.
-spec unary_op_arg_type(atom(), type()) -> type().
unary_op_arg_type('+', Ty) -> Ty;
unary_op_arg_type(Op, {type, _, union, Tys}) ->
    type(union, [ unary_op_arg_type(Op, Ty) || Ty <- Tys ]);
unary_op_arg_type('not', {atom, _, B}) ->
    %% This should only be reachable with B :: boolean(),
    %% as it's checked with glb() before unary_op_arg_type is called.
    B == true orelse B == false orelse erlang:error(unreachable),
    B = ?assert_type(B, boolean()),
    {atom, erl_anno:new(0), not B};
unary_op_arg_type(Op, Ty) when ?is_int_type(Ty), Op == '-' orelse Op == 'bnot' ->
    {Lo, Hi} = gradualizer_int:int_type_to_range(Ty),
    %% TODO: Move this logic to gradualizer_int?
    Neg = fun(pos_inf)             -> neg_inf;
             (neg_inf)             -> pos_inf;
             (N) when Op == '-'    -> -N;
             (N) when Op == 'bnot' -> bnot N end,
    gradualizer_int:int_range_to_type({Neg(Hi), Neg(Lo)});
unary_op_arg_type('-', Ty = {type, _, float, []}) ->
    Ty.

%% Type check list comprehension or a binary comprehension
-spec type_check_comprehension_in(Env        :: env(),
                                  ResTy      :: type(),
                                  OrigExpr   :: gradualizer_type:abstract_expr(),
                                  Compr      :: lc | bc,
                                  Expr       :: gradualizer_type:abstract_expr(),
                                  Position   :: erl_anno:anno(),
                                  Qualifiers :: [ListGen | BinGen | Filter]) ->
        env()
       when
        ListGen :: {generate, erl_anno:anno(), gradualizer_type:abstract_expr(), gradualizer_type:abstract_expr()},
        BinGen  :: {b_generate, erl_anno:anno(), gradualizer_type:abstract_expr(), gradualizer_type:abstract_expr()},
        Filter  :: gradualizer_type:abstract_expr().
type_check_comprehension_in(Env, ResTy, OrigExpr, lc, Expr, _P, []) ->
    case expect_list_type(ResTy, allow_nil_type, Env) of
        any ->
            {_Ty, _VB} = type_check_expr(Env, Expr),
            Env;
        {elem_ty, ElemTy} ->
            _VB = type_check_expr_in(Env, ElemTy, Expr),
            Env;
        {elem_tys, ElemTys} ->
            type_check_union_in(Env, ElemTys, Expr);
        {type_error, _} ->
            throw(type_error(OrigExpr, type(list), ResTy))
    end;
type_check_comprehension_in(Env, ResTy, OrigExpr, bc, Expr, _P, []) ->
    case expect_binary_type(ResTy, Env) of
        any ->
            {_Ty, _VB} = type_check_expr(Env, Expr),
            Env;
        {elem_ty, ElemTy} ->
            ExprTy = case ElemTy of
                         {type, _, binary, [{integer, _, 0}, {integer, _, _N}]} ->
                             %% The result is a multiple of N bits.
                             %% Expr must be a multiple of N bits too.
                             ElemTy;
                         {type, _, binary, [{integer, _, M}, {integer, _, _N}]}
                           when M > 0 ->
                             %% The result is a binary with a minimum size of M. This
                             %% requires that the generators are non-empty. We don't
                             %% check that. At least, we can check that Gen is a
                             %% bitstring.
                             %% TODO: Actually typecheck this case
                             {type, erl_anno:new(0), binary,
                              [{integer, erl_anno:new(0), 0},
                               {integer, erl_anno:new(0), 1}]};
                         _ ->
                             Ty = {type, erl_anno:new(0), binary,
                                   [{integer, erl_anno:new(0), 0},
                                    {integer, erl_anno:new(0), 1}]},
                             throw({type_error, OrigExpr, Ty, ElemTy})
                     end,
            _VB = type_check_expr_in(Env, ExprTy, Expr),
            Env;
        {elem_tys, ElemTys} ->
            type_check_union_in(Env, ElemTys, Expr);
        {type_error, Ty} ->
            throw({type_error, OrigExpr, Ty, ResTy})
    end;
type_check_comprehension_in(Env, ResTy, OrigExpr, Compr, Expr, P,
                            [{generate, _, Pat, Gen} | Quals]) ->
    {Ty, _VB1} = type_check_expr(Env, Gen),
    case expect_list_type(Ty, allow_nil_type, Env) of
        any ->
            NewEnv = add_any_types_pat(Pat, Env),
            _VB2 = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
            Env;
        {elem_ty, ElemTy} ->
            {_PatTys, _UBounds, NewEnv} =
                add_types_pats([Pat], [ElemTy], Env, capture_vars),
            _VB2 = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
            Env;
        {elem_tys, _ElemTys} ->
            %% TODO: As a hack, we treat a union type as any, just to
            %% allow the program to type check.
            NewEnv = add_any_types_pat(Pat, Env),
            _VB2 = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
            Env;
        {type_error, BadTy} ->
            throw(type_error(Gen, BadTy, type(list)))
    end;
type_check_comprehension_in(Env, ResTy, OrigExpr, Compr, Expr, P,
                            [{b_generate, _P_Gen, Pat, Gen} | Quals]) ->
    %% Binary generator: Pat <= Gen
    %% Gen and Pat should be bitstrings (of any size).
    BitTy = {type, erl_anno:new(0), binary,
                   [{integer, erl_anno:new(0), 0},
                    {integer, erl_anno:new(0), 1}]},
    _VarBindsGen = type_check_expr_in(Env, BitTy, Gen),
    {_PatTys, _UBounds, NewEnv} =
        add_types_pats([Pat], [BitTy], Env, capture_vars),
    type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals);
type_check_comprehension_in(Env, ResTy, OrigExpr, Compr, Expr, P, [Pred | Quals]) ->
    %% We choose to check the type of the predicate here. Arguments can be
    %% made either way on whether we should check the type here.
    %% TODO: As a non-boolean predicates don't give runtime errors, we could
    %% possibly add a configuration parameter to toggle this check.
    VB1 = type_check_expr_in(Env, {type, erl_anno:new(0), 'boolean', []}, Pred),
    Env1 = union_var_binds([VB1], Env),
    VB2 = type_check_comprehension_in(Env1, ResTy, OrigExpr, Compr, Expr, P, Quals),
    union_var_binds([VB2], Env1).

-spec type_check_assocs(env(), _) -> {[type()], env()}.
type_check_assocs(Env, [{Assoc, _P, Key, Val}| Assocs])
  when Assoc == map_field_assoc orelse Assoc == map_field_exact ->
    {KeyTy, _KeyVB} = type_check_expr(Env, Key),
    {ValTy, _ValVB} = type_check_expr(Env, Val),
    {AssocTys, VB}   = type_check_assocs(Env, Assocs),
    {[type(Assoc, [KeyTy, ValTy]) | AssocTys], VB};
type_check_assocs(Env, []) ->
    {[], Env}.

-spec update_map_type(type(), [type()]) -> type().
update_map_type(?type(Ty, Arg), AssocTys)
    when Ty == map, Arg == any;
         Ty == any, Arg == [] ->
    %% The original type could have any keys
    %% so we also need to include an optional any() => any() association
    %% in the updated map type.
    %% Map key types can be overlapping, precedence is left to right,
    %% so let's append it as the last entry.
    UpdatedAssocs = update_assocs(AssocTys, []) ++ [type(map_field_assoc, [type(any), type(any)])],
    type(map, UpdatedAssocs);
update_map_type(?type(map, Assocs), AssocTys) ->
    %% `AssocTys' come from a map creation or map update expr - after the expr is evaluated the map
    %% will contain the associations. Therefore in the resulting map type they
    %% cannot be optional - we rewrite optional assocs to non-optional ones.
    UpdatedAssocs = update_assocs(AssocTys, Assocs),
    type(map, UpdatedAssocs);
update_map_type({var, _, _Var}, _AssocTys) ->
    type(any);
update_map_type(?type(union, MapTys), AssocTys) ->
    type(union, [update_map_type(MapTy, AssocTys) || MapTy <- MapTys]);
update_map_type(Type, _AssocTys) ->
    throw(illegal_map_type(Type)).

%% Override existing key's value types and append those key types
%% which are not updated
-spec update_assocs(_, _) -> [type()].
update_assocs([{type, _, _Assoc, [Key, ValueType]} | AssocTys],
	      Assocs) ->
    [type(map_field_exact, [Key, ValueType]) |
     case take_assoc(Key, Assocs, []) of
         {value, _, RestAssocs} ->
             update_assocs(AssocTys, RestAssocs);
         false ->
             %% TODO check if we are updating an existing Key (:=)
             update_assocs(AssocTys, Assocs)
     end];
update_assocs([], Assocs) ->
    Assocs.

-spec take_assoc(_, _, _) -> {value, type(), [type()]} | false.
take_assoc(Key, [Assoc = ?type(_, [Key, _VTy])|T], L) ->
    {value, Assoc, lists:reverse(L, T)};
take_assoc(Key, [H|Assocs], L) ->
    take_assoc(Key, Assocs, [H|L]);
take_assoc(_, [], _) ->
    false.

-spec type_check_fun(env(), expr(), arity()) -> {[type()], env()}.
type_check_fun(Env, {atom, P, Name}, Arity) ->
    % Local function call
    Types = get_bounded_fun_type_list(Name, Arity, Env, P),
    {Types, Env};
type_check_fun(Env, {remote, P, {atom,_,Module}, {atom,_,Fun}}, Arity) ->
    % Module:function call
    case gradualizer_db:get_spec(Module, Fun, Arity) of
        {ok, Types} -> {Types, Env};
        not_found   -> throw(call_undef(P, Module, Fun, Arity))
    end;
type_check_fun(Env, {remote, _, _Expr, _}, Arity) ->
    % Call to an unknown module. Revert to dynamic types.
    P = erl_anno:new(0),
    AnyArgs = ?assert_type(lists:duplicate(Arity, type(any)), [type()]),
    FunTy = {type, P, bounded_fun,
             [{type, P, 'fun',
               [{type, P, product, AnyArgs}, type(any)]},
              []]},
    {[FunTy], Env};
type_check_fun(Env, Expr, _Arity) ->
    case type_check_expr(Env, Expr) of
        {Type, Env1} ->
            ?assert(not is_list(Type)),
            {[Type], Env1}
    end.

-spec type_check_call_intersection(env(), type(), _, _, _, _) -> env().
type_check_call_intersection(Env, ResTy, OrigExpr, ClauseTys, Args, {P, Name, FunTy}) ->
    {FunResTy, Env1} = type_check_call_ty_intersect(Env, ClauseTys, Args, {Name, P, FunTy}),
    case subtype(FunResTy, ResTy, Env) of
        true ->
            union_var_binds([Env1], Env);
        false ->
            throw(type_error(OrigExpr, FunResTy, ResTy))
    end.

-spec check_call_arity(_, _, _) -> ok.
check_call_arity({fun_ty, ArgsTy, _FunResTy}, Args, {P, Name, _}) ->
    case length(ArgsTy) =:= length(Args) of
        true -> ok;
        false ->
            LenTys = arity(length(ArgsTy)),
            LenArgs = arity(length(Args)),
            throw(type_error(call_arity, P, Name, LenTys, LenArgs))
    end.

-spec type_check_call(env(), type(), _, _, _, _) -> env().
type_check_call(_Env, _ResTy, _, {fun_ty, ArgsTy, _FunResTy}, Args, {P, Name, _})
        when length(ArgsTy) /= length(Args) ->
    LenTys = arity(length(ArgsTy)),
    LenArgs = arity(length(Args)),
    throw(type_error(call_arity, P, Name, LenTys, LenArgs));
type_check_call(Env, ResTy, OrigExpr, {fun_ty, ArgsTy, FunResTy} = FunTy, Args, {P, Name, OrigFunTy}) ->
    {CallResTy, VarBinds} =
        case lists:any(fun contains_type_variables/1, [ResTy | ArgsTy]) of
            false ->
                VarBindsList =
                    lists:zipwith(fun (ArgTy, Arg) ->
                                            type_check_expr_in(Env, ArgTy, Arg)
                                    end, ArgsTy, Args),
                {FunResTy, union_var_binds(VarBindsList, Env)};
            true ->
                type_check_poly_call(Env, FunTy, Args, {Name, P, OrigFunTy})
        end,
    case subtype(CallResTy, ResTy, Env) of
        true ->
            VarBinds;
        false ->
            throw(type_error(OrigExpr, CallResTy, ResTy))
    end;
type_check_call(Env, ResTy, OrigExpr, {fun_ty_intersection, Tys}, Args, E) ->
    type_check_call_intersection(Env, ResTy, OrigExpr, Tys, Args, E);
type_check_call(Env, ResTy, OrigExpr, {fun_ty_union, Tys}, Args, E) ->
    type_check_call_union(Env, ResTy, OrigExpr, Tys, Args, E);
type_check_call(_Env, _ResTy, _, {type_error, _}, _Args, {_, Name, FunTy}) ->
    throw(type_error(Name, FunTy, type('fun'))).


-spec type_check_call_union(env(), _, _, _, _, _) -> env().
type_check_call_union(Env, _ResTy, _, [], _Args, _E) ->
    Env;
type_check_call_union(Env, ResTy, OrigExpr, [Ty|Tys], Args, E) ->
    VB1 = type_check_call(Env, ResTy, OrigExpr, Ty, Args, E),
    VB2 = type_check_call_union(Env, ResTy, OrigExpr, Tys, Args, E),
    %% TODO: It's not clear to me what should be returned here.
    %% When combining all the varbinds we should really create
    %% a union of all types for a variable.
    union_var_binds(VB1, VB2, Env).



-spec type_check_block(env(), [expr()]) -> {type(), env()}.
type_check_block(Env, [Expr]) ->
    type_check_expr(Env, Expr);
type_check_block(Env, [Expr | Exprs]) ->
    {_, VarBinds} = type_check_expr(Env, Expr),
    {Ty, VB} = type_check_block(add_var_binds(Env, VarBinds, Env), Exprs),
    {Ty, add_var_binds(VB, VarBinds, Env)}.

-spec type_check_block_in(env(), type(), [expr()]) -> env().
type_check_block_in(Env, ResTy, [Expr]) ->
    type_check_expr_in(Env, ResTy, Expr);
type_check_block_in(Env, ResTy, [Expr | Exprs]) ->
    {_, VarBinds} = type_check_expr(Env, Expr),
    VB = type_check_block_in(add_var_binds(Env, VarBinds, Env), ResTy, Exprs),
    add_var_binds(VB, VarBinds, Env).

-spec type_check_union_in(env(), [type()], expr()) -> env().
type_check_union_in(Env, Tys, Expr) ->
    case type_check_union_in1(Env, Tys, Expr) of
        none        -> throw(type_error(mismatch, type(union, Tys), Expr));
        Env2         -> Env2
    end.

-spec type_check_union_in1(env(), [type()], expr()) -> env() | none.
type_check_union_in1(Env, [Ty|Tys], Expr) ->
    try
        type_check_expr_in(Env, Ty, Expr)
    catch
        E when element(1,E) == type_error ->
            type_check_union_in1(Env, Tys, Expr)
    end;
type_check_union_in1(_Env, [], _Expr) ->
    none.

-spec type_check_tuple_union_in(env(), [[type()]], [expr()]) -> R when
      R :: [env()] | none.
type_check_tuple_union_in(Env, [Tys|Tyss], Elems) ->
    try
        [type_check_expr_in(Env, Ty, Expr)
       || {Ty, Expr} <- lists:zip(Tys, Elems)]
    catch
        E when element(1,E) == type_error ->
            type_check_tuple_union_in(Env, Tyss, Elems)
    end;
type_check_tuple_union_in(_Env, [], _Elems) ->
    none.

-spec get_bounded_fun_type_list(atom(), arity(), env(), anno()) -> [type()].
get_bounded_fun_type_list(Name, Arity, Env, P) ->
    case maps:find({Name, Arity}, Env#env.fenv) of
        {ok, Types} when is_list(Types) ->
            [ typelib:remove_pos(Ty) || Ty <- Types ];
        error ->
            case erl_internal:bif(Name, Arity) of
                true ->
                    {ok, Types} = gradualizer_db:get_spec(erlang, Name, Arity),
                    Types;
                false ->
                    %% If it's not imported, the file doesn't compile.
                    case get_imported_bounded_fun_type_list(Name, Arity, Env, P) of
                        {ok, Types} ->
                            Types
                    end
            end
    end.

-spec get_imported_bounded_fun_type_list(atom(), arity(), env(), anno()) -> {ok, [type()]} | error.
get_imported_bounded_fun_type_list(Name, Arity, Env, P) ->
    case maps:find({Name, Arity}, Env#env.imported) of
        {ok, Module} ->
            case gradualizer_db:get_spec(Module, Name, Arity) of
                {ok, BoundedFunTypeList} ->
                    {ok, BoundedFunTypeList};
                not_found ->
                    throw(call_undef(P, Module, Name, Arity))
            end;
        error -> error
    end.

-spec get_atom(env(), _) -> gradualizer_type:af_atom() | false.
get_atom(_Env, Atom = {atom, _, _}) ->
    Atom;
get_atom(Env, {var, _, Var}) ->
    case maps:get(Var, Env#env.venv) of
	Atom = {atom, _, _} ->
	    Atom;
	_ ->
	    false
    end;
get_atom(_Env, _) ->
    false.

%% Turns all type variables in a given type into rigid type variables.
%%
%% We want to use rigid type variables when typechecking a polymorphic
%% function because type variables used in its spec can be instantiated
%% to anything by the caller, so the polymorphic function cannot make
%% any assumptions about their type or value.
%%
%% Variables inside function constraints (e.g., `List' in the type fragment
%% `when List :: [term()]') are kept as they are, i.e., `var'.
-spec make_rigid_type_vars(type()) -> type();
                          ([type()]) -> [type()];
                          (fun_ty()) -> fun_ty().
make_rigid_type_vars(Tys) when is_list(Tys) ->
    [ make_rigid_type_vars(Ty) || Ty <- Tys ];
make_rigid_type_vars({var, _, '_'} = T) ->
    T;
make_rigid_type_vars({var, P, Var}) when is_atom(Var) ->
    {rigid_var, P, Var};
make_rigid_type_vars({type, _, constraint, _Args} = T) ->
    % do no descent into function constraints (bounds)
    T;
make_rigid_type_vars({type, P, Tag, Args}) ->
    {type, P, Tag, make_rigid_type_vars(Args)};
make_rigid_type_vars({user_type, P, Tag, Args}) ->
    {user_type, P, Tag, make_rigid_type_vars(Args)};
make_rigid_type_vars({remote_type, P, [Mod, Fun, Args]}) ->
    {remote_type, P, [Mod, Fun, make_rigid_type_vars(Args)]};
make_rigid_type_vars({ann_type, P, [AnnoVar, Type]}) ->
    {ann_type, P, [AnnoVar, make_rigid_type_vars(Type)]};
make_rigid_type_vars({fun_ty, ArgTys, ResTy}) ->
    {fun_ty, make_rigid_type_vars(ArgTys), make_rigid_type_vars(ResTy)};
make_rigid_type_vars({fun_ty_intersection, FunTys}) ->
    {fun_ty_intersection, make_rigid_type_vars(FunTys)};
make_rigid_type_vars({fun_ty_union, FunTys}) ->
    {fun_ty_union, make_rigid_type_vars(FunTys)};
make_rigid_type_vars(T) ->
    T.

%% Infers (or at least propagates types from) fun/receive/try/case/if clauses.
-spec infer_clauses(env(), [gradualizer_type:abstract_clause()]) -> R when
      R :: {type(), VarBinds :: env()}.
infer_clauses(Env, Clauses) ->
    {Tys, VarBindsList} = lists:unzip(lists:map(fun (Clause) ->
                                                        infer_clause(Env, Clause)
                                                end, Clauses)),
    {normalize(type(union, Tys), Env),
     union_var_binds(VarBindsList, Env)}.

-spec infer_clause(env(), gradualizer_type:abstract_clause()) -> R when
      R :: {type(), VarBinds :: env()}.
infer_clause(Env, {clause, _, Args, Guards, Block}) ->
    EnvNew = add_any_types_pats(Args, Env),
    % TODO: Can there be variable bindings in a guard? Right now we just
    % discard them.
    % TODO: Should we check that guards return boolean()?
    lists:map(fun (GuardConj) ->
                      lists:map(fun (Guard) ->
                                        type_check_expr(EnvNew, Guard)
                                end, GuardConj)
              end, Guards),
    {Ty, VB} = type_check_block(EnvNew, Block),
    {Ty, union_var_binds(VB, EnvNew, EnvNew)}.

-spec check_clauses_fun(Env, FunTy, Clauses) -> R when
      Env :: env(),
      FunTy :: fun_ty(),
      Clauses :: [gradualizer_type:abstract_clause(), ...],
      R :: env().
check_clauses_fun(Env, {fun_ty, ArgsTy, FunResTy}, Clauses) ->
    check_clauses(Env, ArgsTy, FunResTy, Clauses, bind_vars);
check_clauses_fun(Env, {fun_ty_intersection, Tys}, Clauses) ->
    check_clauses_intersect(Env, Tys, Clauses);
check_clauses_fun(Env, {fun_ty_union, Tys}, Clauses) ->
    check_clauses_union(Env, Tys, Clauses).

-spec check_clauses_intersect(env(), [fun_ty_simple()], Clauses) -> env() when
      Clauses :: [gradualizer_type:abstract_clause(), ...].
check_clauses_intersect(Env, Tys, Clauses) ->
    FunTys = lists:map(fun ({fun_ty, ArgsTys, ResTy}) ->
                               {ArgsTys, ResTy}
                       end, Tys),
    RefinedArgsTyss = maps:from_list(lists:map(fun ({ArgsTys, _}) ->
                                                       {ArgsTys, ArgsTys}
                                               end, FunTys)),
    check_clauses_intersection(Env, FunTys, {Clauses, #{}, RefinedArgsTyss}, Clauses, bind_vars).

-spec check_clauses_union(env(), [fun_ty()], Clauses) -> env() when
      Clauses :: [gradualizer_type:abstract_clause(), ...].
check_clauses_union(_Env, [], _Clauses) ->
    %% TODO: Improve quality of type error
    throw(type_error(check_clauses));
check_clauses_union(Env, [Ty|Tys], Clauses) ->
    try
        check_clauses_fun(Env, Ty, Clauses)
    catch
        Error when element(1,Error) == type_error ->
            check_clauses_union(Env, Tys, Clauses)
    end.

%% Checks a list of function clauses against an intersection type (multiple-clause spec).
-spec check_clauses_intersection(Env, SpecClauses, Acc, Clauses, Caps) -> Env when
      Env :: env(),
      SpecClauses :: [{[type()], type()}],
      Acc :: {OrigClauses, Seen, RefinedArgsTyss},
      OrigClauses :: [gradualizer_type:abstract_clause(), ...],
      Seen :: map(),
      RefinedArgsTyss :: #{[type()] := [type()]},
      Clauses :: [gradualizer_type:abstract_clause()],
      Caps :: capture_vars | bind_vars.
check_clauses_intersection(Env, [] = _SpecClauses, _Acc, _Clauses, _Caps) ->
    %% `SpecClauses' are empty - the function is checked without errors.
    Env;
check_clauses_intersection(Env, [{ArgsTys, _ResTy} = SpecClause | SpecClauses],
                           {OrigClauses, Seen, RefinedArgsTyss},
                           [] = _Clauses, _Caps) ->
    %% `SpecClauses' are not empty, but `Clauses' are empty.
    %% Let's consider the following code:
    %%
    %% -spec h(t()) -> string();
    %%        ([t()]) -> [string()].
    %% h(Types) when is_list(Types) -> lists:map(fun h/1, Types);
    %% h({tag, _}) -> "".
    %%
    %% We'll reach this `check_clauses_intersection' clause when we've already
    %% checked `(t()) -> string()' and we're looking at `([t()]) -> string()'.
    %% In order to check `(t()) -> string()' we had to go through all `h/1' clauses,
    %% so `Clauses' is empty.
    %% To find clauses matching `([t()]) -> string()' we have to start over with `OrigClauses'.
    check_clauses_intersection(Env, [SpecClause | SpecClauses],
                               {OrigClauses, maps:put(ArgsTys, hd(OrigClauses), Seen), RefinedArgsTyss},
                               OrigClauses, _Caps);
check_clauses_intersection(Env, [{ArgsTys, ResTy} = SpecClause | SpecClauses],
                           {OrigClauses, Seen, RefinedArgsTyss},
                           [Clause | Clauses], Caps) ->
    %% `SpecClauses' is not empty and `Clauses' is not empty either.
    %% Now considering:
    %%
    %% -spec g2(t()) -> string();
    %%         ([t()]) -> [string()].
    %% g2([] = _Types) -> [];
    %% g2({tag, _}) -> "";
    %% g2([_|_] = Types) -> lists:map(fun g2/1, Types).
    %%
    %% We're in the general case, for example checking
    %% `g2([_|_] = _Types) -> []' against `([t()]) -> [string()]'.
    %%
    %% First, we have to check if there's already any refinement recorded for `ArgsTys :: [t()]'.
    MaybeRefinedArgsTys = maps:get(ArgsTys, RefinedArgsTyss),
    %% We also have to check if we've already seen this `{ArgsTys, Clause}' pair.
    %% If that's the case, we have to fail to prevent cycling indefinitely.
    case check_clauses_intersection_throw_if_seen(ArgsTys, MaybeRefinedArgsTys, Clause, Seen, Env) of
        {type_error, E} ->
            throw(E);
        ok ->
            Env1 = push_clauses_controls(Env, #clauses_controls{exhaust = Env#env.exhaust}),
            try check_clause(Env1, MaybeRefinedArgsTys, ResTy, Clause, Caps) of
                {[?type(none) | _] = RefinedArgsTys, _Env2} ->
                    %% We've exhausted this `SpecClause'. Check remaining `SpecClauses'.
                    RefinedArgsTyss1 = maps:put(ArgsTys, RefinedArgsTys, RefinedArgsTyss),
                    Seen1 = maps:put({ArgsTys, Clause}, true, Seen),
                    check_clauses_intersection(pop_clauses_controls(Env1),
                                               SpecClauses,
                                               {OrigClauses, Seen1, RefinedArgsTyss1},
                                               OrigClauses, Caps);
                {RefinedArgsTys, _Env2} ->
                    %% We've not exhausted this `SpecClause', so continue with the next `Clauses'.
                    RefinedArgsTyss1 = maps:put(ArgsTys, RefinedArgsTys, RefinedArgsTyss),
                    Seen1 = maps:put({ArgsTys, Clause}, true, Seen),
                    check_clauses_intersection(pop_clauses_controls(Env1),
                                               [SpecClause | SpecClauses],
                                               {OrigClauses, Seen1, RefinedArgsTyss1},
                                               Clauses, Caps)
            catch
                E when element(1, E) =:= type_error ->
                    %% We've not exhausted this spec clause, but we've got a type error,
                    %% e.g. a pattern in the function head doesn't match the spec.
                    %% We'll rethrow this error if we ever cycle back to this
                    %% `SpecClause' / `Clause' combination.
                    Seen1 = maps:put({ArgsTys, Clause}, E, Seen),
                    check_clauses_intersection(pop_clauses_controls(Env1),
                                               [SpecClause | SpecClauses],
                                               {OrigClauses, Seen1, RefinedArgsTyss},
                                               Clauses, Caps)
            end
    end.

%% Checks a list of clauses (if/case/fun/try/catch/receive) against a single-clause spec.
-spec check_clauses(Env, ArgsTy, ResTy, Clauses, Caps) -> Env when
      Env :: env(),
      ArgsTy :: [type()],
      ResTy :: type(),
      Clauses :: [gradualizer_type:abstract_clause()],
      Caps :: capture_vars | bind_vars.
check_clauses(Env, ArgsTy, ResTy, Clauses, Caps) ->
    Env1 = push_clauses_controls(Env, #clauses_controls{exhaust = Env#env.exhaust}),
    %% Clauses for if, case, functions, receive, etc.
    {VarBindsList, RefinedArgsTy, Env2} =
        check_reachable_clauses(ResTy, Clauses, Caps, [], ArgsTy, Env1),
    check_arg_exhaustiveness(Env2, ArgsTy, Clauses, RefinedArgsTy),
    Env3 = pop_clauses_controls(Env2),
    union_var_binds(VarBindsList, Env3).

-spec check_clauses_intersection_throw_if_seen(_, _, _, _, _) -> ok | {type_error, _}.
check_clauses_intersection_throw_if_seen(ArgsTys, RefinedArgsTy, Clause, Seen, Env) ->
    case maps:get({ArgsTys, Clause}, Seen, false) of
        false ->
            ok;
        true ->
            {clause, P, _, _, _} = Clause,
            throw(nonexhaustive(P, gradualizer_lib:pick_values(RefinedArgsTy, Env)));
        ClauseError ->
            {type_error, ClauseError}
    end.

-spec check_reachable_clauses(type(), Clauses, Caps, [env()], [type()], env()) -> R when
      Clauses :: [gradualizer_type:abstract_clause()],
      Caps :: capture_vars | bind_vars,
      R :: {[env()],
            [type()],
            env()}.
check_reachable_clauses(_ResTy, [], _Caps, VBs, RefinedArgsTys, Env) ->
    {VBs, RefinedArgsTys, Env};
check_reachable_clauses(_ResTy, Clauses, _Caps, _, [?type(none)|_], _Env) ->
    Clauses = ?assert_type(Clauses, [gradualizer_type:abstract_clause(), ...]),
    throw(type_error(unreachable_clauses, element(2, hd(Clauses))));
check_reachable_clauses(ResTy, [Clause | Clauses], Caps, VBs, RefinedArgsTys, EnvIn) ->
    {NewRefinedArgsTys, Env2} = check_clause(EnvIn, RefinedArgsTys, ResTy, Clause, Caps),
    VB = refine_vars_by_mismatching_clause(Clause, EnvIn#env.venv, Env2),
    check_reachable_clauses(ResTy, Clauses, Caps, [Env2 | VBs],
                            NewRefinedArgsTys, Env2#env{venv = VB}).

-spec push_clauses_controls(env(), #clauses_controls{}) -> env().
push_clauses_controls(#env{} = Env, #clauses_controls{} = CC) ->
    ?verbose(Env, "Pushing ~p~n", [CC]),
    CStack = Env#env.clauses_stack,
    Env#env{clauses_stack = [CC | CStack]}.

-spec pop_clauses_controls(env()) -> env().
pop_clauses_controls(#env{} = Env) ->
    ?verbose(Env, "Popping clauses controls~n", []),
    [_ | CStack] = Env#env.clauses_stack,
    Env#env{clauses_stack = CStack}.

-spec disable_exhaustiveness_check(env()) -> env().
disable_exhaustiveness_check(#env{} = Env) ->
    [CC | CStack] = Env#env.clauses_stack,
    NewCC = CC#clauses_controls{exhaust = false},
    ?verbose(Env, "Disabling exhaustiveness checking for this clauses list~n", []),
    Env#env{clauses_stack = [NewCC | CStack]}.

%% @doc Check pattern matching exhaustiveness of a function or case expression.
%%
%% This only works if all the arguments are typed, i.e. not `any()'.
%% To be more precise, it only works if all the arguments are `refinable()'.
%%
%% `RefinedArgTys' is the difference of `ArgTys' and all the patterns matched by `Clauses'.
%% If `Clauses' completely cover `ArgTys', that is the function heads or case expression
%% cover(s), aka exhaust(s), all possible cases, `RefinedArgTys' is empty
%% (it's a list of `type(none)'s).
%%
%% For `case' expressions, `ArgTys' and `RefinedArgTys' are single-element lists
%% (that is, a `case' expression has only one _argument_).
%% For functions, these lists are as long as the function arity.
%%
%% Currently, exhaustiveness checking is disabled if a clause has any guards.
%% TODO: Exhaustiveness checking might be improved in the future to handle (some) guards.
%% @end
-spec check_arg_exhaustiveness(env(), [type()], Clauses, [type()]) -> ok when
      Clauses :: [gradualizer_type:abstract_clause()].
check_arg_exhaustiveness(Env, ArgTys, Clauses, RefinedArgTys) ->
    case exhaustiveness_checking(Env) andalso
         all_refinable(ArgTys, Env) andalso
         some_type_not_none(RefinedArgTys)
    of
        true ->
            [{clause, P, _, _, _} | _] = Clauses,
            throw(nonexhaustive(P, gradualizer_lib:pick_values(RefinedArgTys, Env)));
        _ ->
            ok
    end.

-spec exhaustiveness_checking(env()) -> boolean().
exhaustiveness_checking(#env{} = Env) ->
    [#clauses_controls{} = B | _] = Env#env.clauses_stack,
    Exhaust = B#clauses_controls.exhaust,
    ?verbose(Env, "Exhaustiveness checking: ~p~n", [Exhaust]),
    Exhaust.

-spec all_refinable(_, env()) -> boolean().
all_refinable(any, _Env) -> false;
all_refinable(Types, Env) -> lists:all(fun (Ty) -> refinable(Ty, Env) end, Types).

-spec some_type_not_none([type()]) -> boolean().
some_type_not_none(Types) when is_list(Types) ->
    lists:any(fun (T) -> T =/= type(none) end, Types).

%% This function checks clauses.
%% * If clauses have 0 arguments;
%% * case/try/catch/receive clauses have 1 argument;
%% * function clauses have any number of arguments;
%% * the patterns for catch C:E:T is represented as {C,E,T}
-spec check_clause(env(), [type()], type(), gradualizer_type:abstract_clause(),
		   capture_vars | bind_vars) ->
        {RefinedTys :: [type()] , VarBinds :: env()}.
check_clause(Env, ArgsTy, ResTy, C = {clause, P, Args, Guards, Block}, Caps) ->
    ?verbose(Env, "~sChecking clause :: ~s~n", [gradualizer_fmt:format_location(C, brief), typelib:pp_type(ResTy)]),
    case {length(ArgsTy), length(Args)} of
        {L, L} ->
            {PatTys, _UBounds, EnvNew} = add_types_pats(Args, ArgsTy, Env, Caps),
            VarBinds1   = check_guards(EnvNew, Guards),
            EnvNewest   = add_var_binds(EnvNew, VarBinds1, EnvNew),
            VarBinds2 = type_check_block_in(EnvNewest, ResTy, Block),
            RefinedTys1 = refine_clause_arg_tys(ArgsTy, PatTys,
                                                Guards, EnvNewest),
            RefinedTys2 = refine_mismatch_using_guards(RefinedTys1, C,
                                                       Env#env.venv, EnvNewest),
            {RefinedTys2, union_var_binds([VarBinds1, VarBinds2, EnvNewest], EnvNewest)};
        {LenTy, LenArgs} ->
            throw(argument_length_mismatch(P, arity(LenTy), arity(LenArgs)))
    end.
%% DEBUG
%check_clause(_Env, _ArgsTy, _ResTy, Term, _) ->
%    io:format("DEBUG: check_clause term: ~p~n", [Term]),
%    throw(check_clause).

%% Refine types by matching clause. MatchedTys are the types exhausted by
%% each pattern in the previous clause.
-spec refine_clause_arg_tys([type()], [type()], gradualizer_type:af_guard_seq(), env()) -> [type()].
refine_clause_arg_tys(Tys, MatchedTys, [], Env) ->
    Ty        = type(tuple, Tys),
    MatchedTy = type(tuple, MatchedTys),
    case type_diff(Ty, MatchedTy, Env) of
        ?type(tuple, RefTys) ->
            %% type_diff/3 will never return type(tuple, any), so RefTys :: [type()]
            ?assert_type(RefTys, [type()]);
        ?type(none) ->
            lists:duplicate(length(Tys), type(none));
        ?type(union, _) ->
            Tys %% Multiple possibilities => don't refine
    end;
refine_clause_arg_tys(Tys, _MatchedTys, _Guards, _Env) ->
    Tys.

%% Refine a list of types using a list of patterns and guards.
-spec refine_mismatch_using_guards([type()], _, _, env()) -> [type()].
refine_mismatch_using_guards(PatTys,
                             {clause, _,
                              Pats,
                              GuardList,
                              _Block},
                             VEnv, Env) ->
    case GuardList of
        [] ->
            PatTys;
        [Guards] ->
            do_refine_mismatch_using_guards(Guards, PatTys, Pats, VEnv, Env);
        [_|_] ->
            %% we don't know yet how to do this in guard sequences
            Env#env.no_skip_complex_guards orelse throw({skip, too_complex_guards}),
            %% TODO: Invalid lack of pattern refinement
            PatTys
    end.

-spec do_refine_mismatch_using_guards(gradualizer_type:af_guard() | [], [type()], _, _, env()) -> [type()].
do_refine_mismatch_using_guards([], PatTys, _, _, _) -> PatTys;
do_refine_mismatch_using_guards([{call, _, {atom, _, Fun}, Args = [{var, _, Var}]} | Tail],
                                PatTys, Pats, VEnv, Env) ->
    %% A single guard on the form `when is_TYPE(Var)' where Var is one of the
    %% patterns matched against.  If Var was free before the clause (i.e. it
    %% becomes bound in the clause), which failed because of a failing is_TYPE
    %% guard, we can refine the type of that *pattern*.
    PatternCantFail = are_patterns_matching_all_input(Pats, VEnv),
    Result = case {maps:is_key(Var, VEnv), check_guard_call(Fun, Args)} of
                 {false, #{Var := GuardTy}} when PatternCantFail ->
                     %% Find the variable in the list of patterns and refine the
                     %% corresponding type.
                     lists:map(fun ({Ty, {var, _, V}}) when V =:= Var ->
                                       type_diff(Ty, GuardTy, Env);
                                   ({Ty, _}) ->
                                       Ty
                               end,
                               lists:zip(PatTys, Pats));
                 _NoRefinementPossible ->
                     PatTys
             end,
    % Gradualizer fails to deduce that [type() | any()] | [type()] <: [type()],
    % TODO: we should probably normalize an union of lists to a list of unions
    Result = ?assert_type(Result, [type()]),
    do_refine_mismatch_using_guards(Tail, Result, Pats, VEnv, Env);
do_refine_mismatch_using_guards([_ | Tail], PatTys, Pats, VEnv, Env) ->
    do_refine_mismatch_using_guards(Tail, PatTys, Pats, VEnv, Env).

%% Type Difference as in set-difference Ty1 \ Ty2
%% ----------------------------------------------
-spec type_diff(type(), type(), env()) -> type().
type_diff(Ty1, Ty2, Env) ->
    try
        refine(Ty1, Ty2, #{}, Env)
    catch
        no_refinement ->
            %% Imprecision prevents refinement
            Ty1;
        disjoint ->
            %% The types are disjoint.  No refinement.
            %% This can currently also happen due to unhandled type variables, e.g.
            %% T \ {attribute, _TyVar-54982374928, compile, export_all}
            Ty1
    end.

%% Helper for type_diff/3.
%% Normalize, refine by OrigTy \ Ty, revert normalize if result is
%% unchanged.  May throw no_refinement.
-spec refine(type(), type(), map(), env()) -> type().
refine(OrigTy, Ty, Trace, Env) ->
    %% If we're being called recursively and see OrigTy again throw no_refinement.
    %% This is a safeguard similar to the mutual recurson of glb/glb_ty,
    %% or to the stop_refinable_recursion loop breaker.
    case maps:is_key({OrigTy, Ty}, Trace) of
        true ->
            throw(no_refinement);
        false ->
            NormTy = normalize(OrigTy, Env),
            case refine_ty(NormTy, normalize(Ty, Env), maps:put({OrigTy, Ty}, {}, Trace), Env) of
                NormTy -> OrigTy;
                RefTy  -> RefTy
            end
    end.

-spec get_record_fields_types(atom(), erl_anno:anno(), env()) -> [record_field_type()].
get_record_fields_types(Name, Anno, Env) ->
    RecordFields = get_maybe_remote_record_fields(Name, Anno, Env),
    [type_field_type(FieldName, Type) || ?typed_record_field(FieldName, Type) <- RecordFields].

-spec expand_record(atom(), erl_anno:anno(), env()) -> any().
expand_record(Name, Anno, Env) ->
    type_record(Name, get_record_fields_types(Name, Anno, Env)).

%% May throw no_refinement.
-spec refine_ty(type(), type(), #{type() := {}}, env()) -> type().
refine_ty(_Ty, ?type(none), _Trace, _Env) ->
    %% PatTy none() means the pattern can't be used for refinement,
    %% because there is imprecision.
    throw(no_refinement);
refine_ty(?type(T, Args), ?type(T, Args), _Trace, _) ->
    type(none);
refine_ty(?type(record, [{atom, _, Name} | _]), ?type(record, [{atom, _, Name}]), _Trace, _Env) ->
    type(none);
refine_ty(?type(record, [{atom, Anno, Name}]), Refined = ?type(record, [{atom, _, Name} | _]), Trace, Env) ->
    refine_ty(expand_record(Name, Anno, Env), Refined, Trace, Env);
refine_ty(?type(record, [Name | FieldTys1]), ?type(record, [Name | FieldTys2]), Trace, Env)
  when length(FieldTys1) > 0, length(FieldTys1) == length(FieldTys2) ->
    % Record without just the name
    Tys1 = [Ty || ?type_field_type(_, Ty) <- FieldTys1],
    Tys2 = [Ty || ?type_field_type(_, Ty) <- FieldTys2],
    RefTys = [refine(Ty1, Ty2, Trace, Env) || {Ty1, Ty2} <- lists:zip(Tys1, Tys2)],
    RecordsTys = pick_one_refinement_each(Tys1, RefTys),
    RecordsElems = [ lists:map(fun ({Field, RecordTy}) ->
                                       ?type_field_type(FieldName, _) = Field,
                                       type_field_type(FieldName, RecordTy)
                               end, lists:zip(FieldTys1, RecordTys))
                     || RecordTys <- RecordsTys ],
    Records = [type(record, [Name|RecordElems]) || RecordElems <- RecordsElems],
    normalize(type(union, Records), Env);
refine_ty(?type(union, UnionTys), Ty, Trace, Env) ->
    RefTys = lists:foldr(fun (UnionTy, Acc) ->
                             try refine(UnionTy, Ty, Trace, Env) of
                                 RefTy -> [RefTy|Acc]
                             catch
                                 disjoint -> [UnionTy|Acc]
                             end
                          end,
                          [], UnionTys),
    normalize(type(union, RefTys), Env);
refine_ty(Ty, ?type(union, UnionTys), Trace, Env) ->
    %% Union e.g. integer() | float() from an is_number(X) guard.
    %% Refine Ty with each type in the union.
    RefTy = lists:foldl(fun (UnionTy, AccTy) ->
                        try refine(AccTy, UnionTy, Trace, Env) of
                            RefTy -> RefTy
                        catch
                            disjoint -> AccTy
                        end
                end,
                Ty,
                UnionTys),
    %% type() | ... is currently not a subtype of type(),
    %% see test/known_problems/should_pass/different_normalization_levels.erl
    ?assert_type(RefTy, type());
refine_ty(?type(map, _Assocs), ?type(map, [?any_assoc]), _Trace, _Env) ->
    %% #{x => y} \ map() = none()
    type(none);
refine_ty(?type(map, Assocs1) = Ty1, ?type(map, Assocs2) = Ty2, Trace, Env) ->
    %% Ty1 and Ty2 are normalized so they always have a list of assocs.
    Ty1 = ?assert_type(Ty1, {type, anno(), map, [af_assoc_type()]}),
    Ty2 = ?assert_type(Ty2, {type, anno(), map, [af_assoc_type()]}),
    Assocs1 = ?assert_type(Assocs1, [af_assoc_type()]),
    Assocs2 = ?assert_type(Assocs2, [af_assoc_type()]),

    case {has_overlapping_keys(Ty1, Env), has_overlapping_keys(Ty2, Env),
                                          has_non_singleton_required_keys(Ty1)} of
        {false, false, false} ->
            %% The MissingTy is a bit of a hack but it allows us to treat
            %% the case when an assoc is missing as if it was one of the
            %% assoc possible values (which is quite convenient)
            MissingTy = type_atom('__gradualizer_missing_assoc'),

            Pairs = pair_assocs(Assocs1, Assocs2, MissingTy, Env),
            {Keys, Values1, Values2} = lists:unzip3(Pairs),
            Tuple1 = type(tuple, Values1),
            Tuple2 = type(tuple, Values2),

            %% Determines which assocs to keep and what tag to use for them,
            %% and constructs the resulting map type.
            ValuesToMap = fun(?type(tuple, ResValues)) ->
                    KeyValues = lists:zip(Keys, ResValues),
                    ResAssocsRev = lists:foldl(fun({Key, ValOrig}, AssocsIn) ->
                        case process_assoc_value(ValOrig, MissingTy, Env) of
                            {keep, {Tag, Val}} -> [type(Tag, [Key, Val]) | AssocsIn];
                            leave_out -> AssocsIn
                        end
                    end, [], KeyValues),
                    ResAssocs = lists:reverse(ResAssocsRev),
                    type(map, ResAssocs)
            end,

            %% The excellent type_diff algorithm for tuples gives us not only
            %% the comfort of diffing all values at once but also computes
            %% all possible combinations that are covered by the difference.
            %% E.g., #{x := a|b, y => c} \ #{x := a, y := c} should result
            %% in the union of #{x := b, y => c} and #{x := a|b}.
            case refine_ty(Tuple1, Tuple2, Trace, Env) of
                ?type(tuple, _) = Tuple ->
                    ValuesToMap(Tuple);
                ?type(union, Tuples) ->
                    NewTuples = lists:map(ValuesToMap, Tuples),
                    type(union, NewTuples);
                ?type(none) ->
                    type(none)
            end;
        _ ->
            throw(no_refinement)
    end;
refine_ty(?type(tuple, _Tys), ?type(tuple, any), _Trace, _Env) ->
    %% {x,y,z} \ tuple() = none()
    type(none);
refine_ty(?type(tuple, [_|_] = Tys1) = Ty1, ?type(tuple, [_|_] = Tys2) = Ty2, Trace, Env) ->
    Tys1 = ?assert_type(Tys1, [type()]),
    Tys2 = ?assert_type(Tys2, [type()]),
    if
        length(Tys1) /= length(Tys2) ->
            refine_ty_catch_all(Ty1, Ty2, Env);
        length(Tys1) == length(Tys2) ->
            %% Non-empty tuple
            RefTys = [refine(Ty1_, Ty2_, Trace, Env) || {Ty1_, Ty2_} <- lists:zip(Tys1, Tys2)],
            %% {a|b, a|b} \ {a,a} => {b, a|b}, {a|b, b}
            TuplesElems = pick_one_refinement_each(Tys1, RefTys),
            Tuples = [type(tuple, TupleElems) || TupleElems <- TuplesElems],
            normalize(type(union, Tuples), Env)
    end;
refine_ty(?type(tuple, any) = Ty1, ?type(tuple, _) = Ty2, _Trace, Env) ->
    refine_ty_catch_all(Ty1, Ty2, Env);
refine_ty({atom, _, At}, {atom, _, At}, _, _) ->
    type(none);
refine_ty({atom, _, _}, ?type(atom), _, _) ->
    type(none);
refine_ty(?type(list, E), ?type(nil), _, _Env) ->
    type(nonempty_list, E);
refine_ty(?type(list, [ElemTy1]), ?type(nonempty_list, [ElemTy2]), Trace, Env) ->
    case refine(ElemTy1, ElemTy2, Trace, Env) of
        ?type(none) ->
            type(nil);
        RefElemTy ->
            type(list, [RefElemTy])
    end;
refine_ty(?type(nil), ?type(list, _), _, _Env) ->
    type(none);
refine_ty(?type(nonempty_list, _), ?type(list, [?type(any)]), _, _Env) ->
    %% The guard is_list/1 catches every nonempty list
    type(none);
refine_ty(?type(list, _), ?type(list, [?type(any)]), _, _Env) ->
    type(none);
refine_ty(?type(nonempty_list, [Ty1]), ?type(nonempty_list, [Ty2]), Trace, Env) ->
    case refine(Ty1, Ty2, Trace, Env) of
        ?type(none) ->
            type(none);
        RefTy ->
            type(nonempty_list, [RefTy])
    end;
refine_ty(?type(binary, [{integer, _, 0}, {integer, _, N}]),
          ?type(binary, [{integer, _, 0}, {integer, _, 0}]), _, _Env) when N > 0 ->
    %% binary() \ <<>> => nonempty_binary()
    type(binary, [{integer, 0, N}, {integer, 0, N}]);
refine_ty(?type(binary, [{integer, _, 0}, {integer, _, N}]),
          ?type(binary, [{integer, _, N}, {integer, _, N}]), _, _Env) when N > 0 ->
    %% binary() \ nonempty_binary() => <<>>
    type(binary, [{integer, 0, 0}, {integer, 0, 0}]);
refine_ty(?type(binary, [_,_]),
          ?type(binary, [{integer, _, 0}, {integer, _, 1}]), _, _Env) ->
    %% B \ bitstring() => none()
    %% where B is any binary or bitstring type
    type(none);
refine_ty({Tag1, _, M}, {Tag2, _, N}, _, _Env)
    when Tag1 == integer orelse Tag1 == char,
         Tag2 == integer orelse Tag2 == char ->
    if M == N -> type(none);
       M /= N -> throw(disjoint)
    end;
refine_ty(Ty1, Ty2, _, _Env) when ?is_int_type(Ty1),
                                   ?is_int_type(Ty2) ->
    %% TODO: https://github.com/josefs/Gradualizer/issues/406
    gradualizer_int:int_type_diff(Ty1, Ty2);
refine_ty({user_type, Anno, Name, Args}, {user_type, Anno, Name, Args}, _, _Env) ->
    % After being normalized, it's because it's defined as opaque.
    % If it has the same annotation, name and args, it's the same.
    type(none);
refine_ty(Ty1, Ty2, _, Env) ->
    refine_ty_catch_all(Ty1, Ty2, Env).

%% This is the catch-all clause of refine_ty/4.
%% TODO: Since we want to self-check Gradualizer and type-checking guard patterns is not perfect yet,
%% instead of using sophisticated guards not to enter a clause and finally fall into a catch-all
%% we sometimes enter that clause, but then call this function if certain conditions are met.
-spec refine_ty_catch_all(type(), type(), env()) -> type().
refine_ty_catch_all(Ty1, Ty2, Env) ->
    case glb(Ty1, Ty2, Env) of
        ?type(none) -> throw(disjoint);  %% disjoint
        _NotDisjoint -> throw(no_refinement)  %% imprecision
    end.

%% Finds a corresponding assoc from Assocs1 to every assoc from Assocs2.
%%
%% All keys from Assoc1 should be mentioned in Assocs2 if Assocs2 was
%% constructed using add_types_pats (as there is no way how to express
%% that a map shouldn't have a particular field filled in using a pattern).
%% Thus it is safe to reduce the Assocs1 and Assocs2 to just list of Assocs2
%% paired with their corresponding value types in Assocs1. Nothing from
%% Assocs1 should be lost in this way.
-spec pair_assocs([af_assoc_type()], [af_assoc_type()], type(), env())
        -> [{Key :: type(), Value1 :: type(), Value2 :: type()}].
pair_assocs(Assocs1, Assocs2, MissingTy, Env) ->
    %% The MissingTy is later on handled in process_assoc_value.
    AugmentValueTy = fun(map_field_exact, Value)
                            -> Value;
                        (map_field_assoc, Value)
                            -> normalize(type(union, [Value, MissingTy]), Env)
    end,
    Result = lists:map(fun(?type(Tag2, [K2, V2Orig])) ->
        V2 = AugmentValueTy(Tag2, V2Orig),
        case lists:search(fun(?type(_Tag1, [K1, _V1])) ->
            %% Most of the time, the keys will be equal, but the subtype
            %% condition allows us to handle non-singleton keys as well.
            %% For example, both x and y of Assocs2 should pair with x|y
            %% from Assocs1.
            subtype(K2, K1, Env)
        end, Assocs1) of
            {value, ?type(Tag1, [_K1, V1Orig])} ->
                V1 = AugmentValueTy(Tag1, V1Orig),
                {K2, V1, V2};
            false ->
                %% If Assocs2 mentions a key that is not a subtype of any key
                %% from Assocs1, then the corresponding maps are disjoint.
                throw(disjoint)
        end
    end, Assocs2),
    %% type() | ... is currently not a subtype of type(),
    %% see test/known_problems/should_pass/different_normalization_levels.erl
    ?assert_type(Result, [{type(), type(), type()}]).

%% Handles the MissingTy in assoc values and decides whether
%% to keep the assoc and which tag to use for it.
%%
%% MissingTy denotes the possibility of the assoc being left
%% out from the type, so it corresponds to the "=>" assoc.
%% If the value type does not contain MissingTy, it should
%% be the ":=" assoc that must be always present.
%% If the value is precisely MissingTy, it means that the
%% assoc should never be present, and thus should be left out.
-spec process_assoc_value(type(), type(), env())
        -> {keep, {map_field_assoc | map_field_exact, type()}} | leave_out.
process_assoc_value(MissingTy, MissingTy, _Env) -> leave_out;
process_assoc_value(?type(union, ItemsIn), MissingTy, Env) ->
    {Tag, ItemsOutRev} = lists:foldl(
        fun(Item, {Tag, Items}) ->
            if
                Item == MissingTy -> {map_field_assoc, Items};
                true -> {Tag, [Item | Items]}
            end
        end,
        {map_field_exact, []}, ItemsIn),
    ItemsOut = lists:reverse(ItemsOutRev),
    {keep, {Tag, normalize(type(union, ItemsOut), Env)}};
process_assoc_value(Value, _EmptyTyVar, _Env) -> {keep, {map_field_exact, Value}}.

%% Returns a nested list on the form
%%
%%     [[RefTy1, Ty2, Ty3, ..., TyN],
%%      [Ty1, RefTy2, Ty3, ..., TyN],
%%      [Ty1, Ty2, RefTy3, ..., TyN],
%%      ...
%%      [Ty1, Ty2, Ty3, ..., RefTyN]].
%%
%% If RefTyI == none() for any I, that list I is excluded.
-spec pick_one_refinement_each(list(), list()) -> [[type()]].
pick_one_refinement_each([], []) -> [];
pick_one_refinement_each([Ty|Tys], [RefTy|RefTys]) ->
    %% The lists (zero or one list) where we refine head and keep tail
    %% unrefined, if head is possible to refine.
    RefHeadCombinations =
        case RefTy of
            ?type(none) -> [];           %% pattern matches all; no refinement
            _           -> [[RefTy|Tys]] %% refinement possible
        end,
    %% All lists where we keep head and refine one of the rest types
    RefTailCombinations = lists:map(fun (Tail) -> [Ty|Tail] end,
                                    pick_one_refinement_each(Tys, RefTys)),
    %% The last list is the list where to type is refined.
    lists:append(RefHeadCombinations, RefTailCombinations).

%% Is a type refinable to the point that we do exhaustiveness checking on it?
-spec refinable(type(), env()) -> boolean().
refinable(Ty, Env) ->
    refinable(Ty, Env, sets:new()).

-spec refinable(_, _, _) -> boolean().
refinable(?type(integer), _Env, _Trace) ->
    true;
refinable(?type(float), _Env, _Trace) ->
    true;
refinable(?type(char), _Env, _Trace) ->
    true;
refinable(?type(non_neg_integer), _Env, _Trace) ->
    true;
refinable(?type(pos_integer), _Env, _Trace) ->
    true;
refinable(?type(neg_integer), _Env, _Trace) ->
    true;
refinable({atom, _, _}, _Env, _Trace) ->
    true;
refinable(?type(nil), _Env, _Trace) ->
    true;
refinable(?type(Name, Tys) = Ty0, Env, Trace)
  when (tuple =:= Name orelse union =:= Name)
   and is_list(Tys) ->
    case stop_refinable_recursion(Ty0, Env, Trace) of
        stop ->
            true;
        {proceed, NewTrace} ->
            lists:all(fun (Ty) -> refinable(Ty, Env, NewTrace) end, Tys)
    end;
refinable(?type(record, [_ | Fields]) = Ty0, Env, Trace) ->
    case stop_refinable_recursion(Ty0, Env, Trace) of
        stop ->
            true;
        {proceed, NewTrace} ->
            lists:all(fun (Ty) -> refinable(Ty, Env, NewTrace) end,
                      [X || ?type(field_type, X) <- Fields])
    end;
refinable(?type(map, _) = Ty0, Env, Trace) ->
    ?assert_normalized_anno(Ty0),
    ?type(map, Assocs) = Ty = normalize(Ty0, Env),
    %% normalize/2 will not return Assocs=any
    Assocs = ?assert_type(Assocs, [af_assoc_type()]),
    Ty = ?assert_type(Ty, {type, anno(), map, [af_assoc_type()]}),
    case stop_refinable_recursion(Ty, Env, Trace) of
        stop -> true;
        {proceed, NewTrace} ->
            case {has_overlapping_keys(Ty, Env), has_non_singleton_required_keys(Ty)} of
                {false, false} ->
                    lists:all(fun ({type, _, _AssocTag, [KTy, VTy]}) ->
                                      refinable(KTy, Env, NewTrace)
                                      andalso
                                      refinable(VTy, Env, NewTrace)
                              end, Assocs);
                _ ->
                    false
            end
    end;
refinable(?type(string), _Env, _Trace) ->
    true;
refinable(?type(list, [?type(char)]), _Env, _Trace) ->
    true;
refinable(?type(list, [ElemTy]) = Ty, Env, Trace) ->
    case stop_refinable_recursion(Ty, Env, Trace) of
        stop -> true;
        {proceed, NewTrace} ->
            refinable(ElemTy, Env, NewTrace)
    end;
refinable(?type(binary, _), _Env, _Trace) ->
    true;
refinable(?top(), _Env, _Trace) ->
    %% This clause prevents incorrect exhaustiveness warnings
    %% when `gradualizer:top()' is used explicitly.
    false;
refinable(RefinableTy, Env, Trace)
  when element(1, RefinableTy) =:= remote_type; element(1, RefinableTy) =:= user_type ->
    case stop_refinable_recursion(RefinableTy, Env, Trace) of
        stop -> true;
        {proceed, NewTrace} ->
            Opts = case element(1, RefinableTy) of
                       remote_type -> [annotate_user_types];
                       user_type -> []
                   end,
            case gradualizer_lib:get_type_definition(RefinableTy, Env, Opts) of
                {ok, Ty} -> refinable(Ty, Env, NewTrace);
                opaque -> true;
                not_found -> false
            end
    end;
refinable(_, _, _) ->
    false.

%% We're searching down the variants of a recursive type and we've
%% reached this recursive type again (that is, it's found in `Trace').
%% We assume it's refinable to terminate recursion.
%% Refinability will be determined by the variants which are not (mutually) recursive.
-spec stop_refinable_recursion(_, env(), _) -> stop | {proceed, sets:set()}.
stop_refinable_recursion(RefinableTy, Env, Trace) ->
    MTA = mta(RefinableTy, Env),
    case sets:is_element(MTA, Trace) of
        true -> stop;
        false -> {proceed, sets:add_element(MTA, Trace)}
    end.

-spec mta(type(), env()) -> {module(), atom(), non_neg_integer()} | type().
mta({user_type, Anno, Name, Args}, Env) ->
    Module = case typelib:get_module_from_annotation(Anno) of
                 none -> maps:get(module, Env#env.tenv);
                 {ok, M} -> M
             end,
    {Module, Name, length(Args)};
mta(Type, _Env) ->
    Type.

%% Refines the types of bound variables using the assumption that a clause has
%% mismatched.
-spec refine_vars_by_mismatching_clause(gradualizer_type:af_clause(), venv(), env()) -> venv().
refine_vars_by_mismatching_clause({clause, _, Pats, Guards, _Block}, VEnv, Env) ->
    PatternCantFail = are_patterns_matching_all_input(Pats, VEnv),
    case Guards of
        [] ->
            %% No guards, so no refinement
            VEnv;
        [[{call, _, {atom, _, Fun}, Args = [{var, _, Var}]}]] when PatternCantFail ->
            %% Simple case: A single guard on the form `when is_TYPE(Var)'.
            %% If Var was bound before the clause, which failed because of a
            %% failing is_TYPE guard, we can exclude TYPE (as in is_TYPE).
            case {VEnv, check_guard_call(Fun, Args)} of
                {#{Var := Ty}, #{Var := GuardTy}} ->
                    RefinedTy = type_diff(Ty, GuardTy, Env),
                    VEnv#{Var := RefinedTy};
                _ ->
                    %% No refinement
                    VEnv
            end;
        _OtherGuards ->
            Env#env.no_skip_complex_guards orelse throw({skip, too_complex_guards}),
            %% TODO: Invalid lack of refinement
            VEnv
    end.

%% Returns true if a list of patterns match all possible inputs of the same
%% length. For this to be true, all patterns must be distinct free variables or
%% the underscore pattern.
%%
%% See also `should_list_pat_enable_exhaustiveness_check/1', which is similar,
%% but incomplete and specific to list patterns.
-spec are_patterns_matching_all_input([pattern()], map()) -> boolean().
are_patterns_matching_all_input([], _VEnv) ->
    true;
are_patterns_matching_all_input([{var, _, '_'} | Pats], VEnv) ->
    are_patterns_matching_all_input(Pats, VEnv);
are_patterns_matching_all_input([{var, _, V} | Pats], VEnv) ->
    case maps:is_key(V, VEnv) of
        true ->
            false;
        false ->
            %% It doesn't matter which type we set it to. We just need the key.
            are_patterns_matching_all_input(Pats, VEnv#{V => type(any)})
    end;
are_patterns_matching_all_input(_Pat, _VEnv) ->
    false.

-spec check_guard_call(atom(), list()) -> map().
check_guard_call(is_atom, [{var, _, Var}]) -> #{Var => type(atom)};
check_guard_call(is_binary, [{var, _, Var}]) -> #{Var => type(binary)};
check_guard_call(is_bitstring, [{var, _, Var}]) -> #{Var => type(bitstring)};
check_guard_call(is_boolean, [{var, _, Var}]) -> #{Var => type(boolean)};
check_guard_call(is_float, [{var, _, Var}]) -> #{Var => type(float)};
check_guard_call(is_function, [{var, _, Var}]) -> #{Var => type('fun')};
check_guard_call(is_function, [{var, _, Var}, {integer, _, Arity}]) -> #{Var => type_fun(Arity)};
check_guard_call(is_integer, [{var, _, Var}]) -> #{Var => type(integer)};
check_guard_call(is_list, [{var, _, Var}]) -> #{Var => type(list)}; % should this be [top()] instead?
check_guard_call(is_map, [{var, _, Var}]) -> #{Var => type(map, any)};
check_guard_call(is_number, [{var, _, Var}]) -> #{Var => type(number)};
check_guard_call(is_pid, [{var, _, Var}]) -> #{Var => type(pid)};
check_guard_call(is_port, [{var, _, Var}]) -> #{Var => type(port)};
check_guard_call(is_record, [{var, _, Var}, {atom, _, Record}]) -> #{Var => type_record(Record)};
check_guard_call(is_record, [{var, _, Var}, {atom, _, Record}, _]) -> #{Var => type_record(Record)};
check_guard_call(is_reference, [{var, _, Var}]) -> #{Var => type(reference)};
check_guard_call(is_tuple, [{var, _, Var}]) -> #{Var => type(tuple, any)};
check_guard_call(_Fun, _Vars) -> #{}.

%% Derive a type from a comparison against a literal value
%% using Erlang's term order:
%%
%%     number < atom < reference < fun < port < pid <
%%            < tuple < map < nil < list < bitstring
%%
%% Note: This function may return non-standard ranges like 10..pos_inf
-spec type_comp_op(atom(), any()) -> type().
type_comp_op('=<', I) when is_integer(I) ->
    IntTypes = gradualizer_int:int_range_to_types({neg_inf, I}),
    type(union, [type(float) | IntTypes]);
type_comp_op('<', I) when is_integer(I) ->
    type_comp_op('=<', I - 1);
type_comp_op('>=', I) when is_integer(I) ->
    IntTypes = gradualizer_int:int_range_to_types({I, pos_inf}),
    GtTypes = [type(float), type(atom), type(reference), type('fun'),
               type(port), type(pid), type(tuple, any), type(map, any),
               type(list), type(bitstring)],
    type(union, IntTypes ++ GtTypes);
type_comp_op('>', I) when is_integer(I) ->
    type_comp_op('>=', I + 1);
type_comp_op('==', I) when is_integer(I) ->
    type(union, [type(float), {integer, erl_anno:new(0), I}]);
type_comp_op('=:=', I) when is_integer(I) ->
    {integer, erl_anno:new(0), I};
type_comp_op(Op, A) when Op =:= '==' orelse Op =:= '=:=',
                         is_atom(A) ->
    {atom, erl_anno:new(0), A};
type_comp_op(Neq, I) when Neq =:= '/=' orelse Neq =:= '/=',
                          is_integer(I) ->
    {type, _, union, U1} = type_comp_op('<', I),
    {type, _, union, U2} = type_comp_op('>', I),
    type(union, U1 ++ U2);
type_comp_op(_Op, _Val) ->
    %% No refinement / not implemented
    type(any).

%% "Mirrors" a comparison operator to preserve equivalence when swapping the left
%% and right operands. Used for e.g. transforming A < B to B > A.
-spec mirror_comp_op(atom())  -> atom().
mirror_comp_op('<')  -> '>';
mirror_comp_op('>')  -> '<';
mirror_comp_op('>=') -> '=<';
mirror_comp_op('=<') -> '>=';
mirror_comp_op(Comm) -> Comm.

-spec check_guard_expression(env(), term()) -> env().
check_guard_expression(Env, {call, _, {atom, _, Fun}, Vars}) ->
    Env#env{venv = check_guard_call(Fun, Vars)};
check_guard_expression(Env, {call, _, {remote,_, {atom, _, erlang}, {atom, _, Fun}}, Vars}) ->
    Env#env{venv = check_guard_call(Fun, Vars)};
check_guard_expression(Env, {op, _, Op, {var, _, Var}, {Tag, _, Value}})
  when ?is_comp_op(Op), Tag =:= integer orelse Tag =:= atom ->
    Env#env{venv = #{Var => type_comp_op(Op, Value)}};
check_guard_expression(Env, {op, _, Op, {Tag, _, Value}, {var, _, Var}})
  when ?is_comp_op(Op), Tag =:= integer orelse Tag =:= atom ->
    Env#env{venv = #{Var => type_comp_op(mirror_comp_op(Op), Value)}};
check_guard_expression(Env, {op, _OrElseAnno, Op, Call1, Call2}) when Op == 'orelse'; Op == 'or' ->
    G1 = check_guard_expression(Env, Call1),
    G2 = check_guard_expression(Env, Call2),
    union_var_binds_symmetrical([G1, G2], Env);
check_guard_expression(Env, {op, _AndAlsoAnno, Op, Call1, Call2}) when Op == 'andalso'; Op == 'and' ->
    G1 = check_guard_expression(Env, Call1),
    G2 = check_guard_expression(Env, Call2),
    union_var_binds([G1, G2], Env);
check_guard_expression(Env, Guard) ->
    {_Ty, Env1} = type_check_expr(Env, Guard), % Do we need to thread the Env?
    Env1.

%% The different guards use glb
-spec check_guard(env(), list()) -> env().
check_guard(Env, GuardSeq) ->
    RefTys = lists:foldl(fun(Guard, E) ->
                             NEnv = check_guard_expression(E, Guard),
                             %% This is somewhat tricky.
                             %% check_guard_expression/2 will return an env with just
                             %% the types of vars used in the guard expression - we want these to
                             %% refine any types previously in the env.
                             %% We still want to keep types of vars not used in the guard expression,
                             %% though, so we pass both NEnv and E to union_var_binds/2.
                             union_var_binds([NEnv, E], E)
                         end, Env, GuardSeq),
    Env#env{venv = maps:merge(Env#env.venv, RefTys#env.venv)}.

%% TODO: implement proper checking of guards.
-spec check_guards(env(), list()) -> env().
check_guards(Env, []) -> Env;
check_guards(Env, Guards) ->
    Envs = lists:map(fun (Guard) ->
                             check_guard(Env, Guard)
                     end, Guards),
    union_var_binds_symmetrical(Envs, Env).

-spec type_check_function(env(), erl_parse:abstract_form()) -> env().
type_check_function(Env, {function, _Anno, Name, NArgs, Clauses}) ->
    ?verbose(Env, "Checking function ~p/~p~n", [Name, NArgs]),
    case maps:find({Name, NArgs}, Env#env.fenv) of
        {ok, FunTy} ->
            try
                NewEnv = Env#env{current_spec = FunTy},
                FunTyNoPos = [ typelib:remove_pos(?assert_type(Ty, type())) || Ty <- FunTy ],
                Arity = clause_arity(hd(Clauses)),
                case expect_fun_type(NewEnv, FunTyNoPos, Arity) of
                    {type_error, NotFunTy} ->
                        %% This can only happen if `create_fenv/2' creates garbage.
                        erlang:error({invalid_function_type, NotFunTy});
                    FTy ->
                        FTy1 = make_rigid_type_vars(FTy),
                        _Vars = check_clauses_fun(NewEnv, FTy1, Clauses),
                        NewEnv
                end
            catch
                throw:{skip, Reason} ->
                    ?verbose(Env, "Skipping function ~p/~p due to ~s~n", [Name, NArgs, Reason]),
                    Env
            end;
        error ->
            throw(internal_error(missing_type_spec, Name, NArgs))
    end.

-spec clause_arity({clause, _, list(), _, _}) -> arity().
clause_arity({clause, _, Args, _, _}) ->
    arity(length(Args)).

-spec arity(non_neg_integer()) -> arity().
arity(I) ->
    ?assert(I < 256, arity_overflow),
    ?assert_type(I, arity()).

-spec position_info_from_spec(none | form() | forms()) -> erl_anno:anno().
position_info_from_spec(none) ->
    %% This simplifies testing internal functions.
    %% In these cases we don't go through type_check_function,
    %% but call deeper into the typechecker directly.
    erl_anno:new(0);
position_info_from_spec(Form) when is_tuple(Form) ->
    element(2, Form);
position_info_from_spec([_|_] = Forms) ->
    %% TODO: https://github.com/josefs/Gradualizer/issues/388
    position_info_from_spec(hd(Forms)).

%% Type check patterns against types (P1 :: T1, P2 :: T2, ...)
%% and add variable bindings for the patterns.
%% Used for the arguments in clauses and the elements of tuples.
%%
%% The returned lists of types are interpreted like this:
%% PatTy :: Pat as if Pat were a type. For match-all patterns, PatTy
%% is the same as the type. For patterns matching a singleton type, PatTy
%% is the singleton type. Otherwise, PatTy is none(). PatTy is a type exhausted
%% by Pat. UBound is Ty or a subtype such that Pat :: UBound.
-spec add_types_pats(Pats, Tys, Env, Caps) -> R when
      Pats :: [gradualizer_type:abstract_pattern()],
      Tys  :: [type()],
      Env  :: env(),
      Caps :: capture_vars | bind_vars,
      R :: {PatTys      :: [type()],
            UBounds     :: [type()],
            NewEnv      :: env()}.
%% TODO: move tenv to back
add_types_pats(Pats, Tys, Env, Caps) ->
    NewEnv = assign_types_to_vars_bound_more_than_once(Pats, Env, Caps),
    do_add_types_pats(Pats, Tys, NewEnv).

%% Helper for add_types_pats/4, also used in recursive calls from add_type_pat/3.
%%
%% NB: Don't use this function directly. Use add_types_pats/4 instead.
-spec do_add_types_pats(Pats, Tys, Env) -> R when
      Pats :: [gradualizer_type:abstract_pattern()],
      Tys  :: [type()],
      Env  :: env(),
      R :: {PatTys      :: [type()],
            UBounds     :: [type()],
            NewEnv      :: env()}.
do_add_types_pats(Pats, Tys, Env) ->
    add_types_pats(Pats, Tys, Env, [], []).

%% TODO: move tenv to back
-spec add_types_pats(Pats, Tys, Env, PatTysAcc, UBoundsAcc) -> R when
      Pats       :: [gradualizer_type:abstract_pattern()],
      Tys        :: [type()],
      Env        :: env(),
      PatTysAcc  :: [type()],
      UBoundsAcc :: [type()],
      R :: {PatTys      :: [type()],
            UBounds     :: [type()],
            NewEnv      :: env()}.
add_types_pats([], [], Env, PatTysAcc, UBoundsAcc) ->
    {lists:reverse(PatTysAcc), lists:reverse(UBoundsAcc), Env};
add_types_pats([Pat | Pats], [Ty | Tys], Env, PatTysAcc, UBoundsAcc) ->
    NormTy = normalize(Ty, Env),
    {PatTyNorm, UBoundNorm, Env2} =
        ?throw_orig_type(add_type_pat(Pat, NormTy, Env), Ty, NormTy),
    %% De-normalize the returned types if they are the type checked against.
    PatTy  = denormalize(Ty, PatTyNorm, NormTy),
    UBound = denormalize(Ty, UBoundNorm, NormTy),
    add_types_pats(Pats, Tys, Env2, [PatTy|PatTysAcc], [UBound|UBoundsAcc]).

-spec denormalize(type(), type(), type()) -> type().
denormalize(OrigTy, ComputedTy, NormTy) ->
    case ComputedTy of
        NormTy -> OrigTy;
        _      -> ComputedTy
    end.

%% Type check a pattern against a normalized type and add variable bindings.
%%
%% Note 1: For correct variable binding and refinement logic, don't use this
%% function directly.  Instead, use add_types_pats([Pat], [Type], Env, Caps).
%%
%% Note 2: When this function calls itself recursively, take care that the type
%% is normalized first.
-spec add_type_pat(Pat, Type, Env) -> R when
      Pat  :: gradualizer_type:abstract_pattern(),
      Type :: type(),
      Env  :: env(),
      R :: {PatTy :: type(),
            UBound :: type(),
            NewEnv :: env()}.
add_type_pat({var, _, '_'}, Ty, Env) ->
    {Ty, Ty, Env};
add_type_pat({var, _, A} = Var, Ty, Env) ->
    case Env#env.venv of
        #{A := VarTy} ->
            case glb(VarTy, Ty, Env) of
                ?type(none) ->
                    %% TODO: Better type error (it's a pattern, not an expression)
                    throw(type_error(Var, VarTy, Ty));
                RefinedTy ->
                    {type(none), RefinedTy, update_var_type(Env, Var, RefinedTy)}
            end;
        _FreeVar ->
            %% Match all
            {Ty, Ty, set_var_type(Env, Var, Ty)}
    end;
add_type_pat(Pat, ?type(union, _) = UnionTy, Env) ->
    add_type_pat_union(Pat, UnionTy, Env);
add_type_pat(Lit = {Tag, P, Val}, Ty, Env)
  when Tag =:= integer;
       Tag =:= char ->
    LitTy = singleton(integer, Val),
    case subtype(LitTy, Ty, Env) of
        true ->
            {LitTy, LitTy, Env};
        false ->
            throw(type_error(pattern, P, Lit, Ty))
    end;
add_type_pat(Lit = {float, P, _}, Ty, Env) ->
    case subtype(type(float), Ty, Env) of
        true ->
            {type(none), type(float), Env};
        false ->
            throw(type_error(pattern, P, Lit, Ty))
    end;
add_type_pat(Tuple = {tuple, P, Pats}, Ty, Env) ->
    case expect_tuple_type(Ty, length(Pats), Env) of
        any ->
            NewEnv = union_var_binds([ add_any_types_pat(Pat, Env) || Pat <- Pats ], Env),
            {type(none),
             Ty,
             NewEnv};
        {elem_ty, Tys} ->
            {PatTys, UBounds, Env1} = do_add_types_pats(Pats, Tys, Env),
            {type(tuple, PatTys),
             type(tuple, UBounds),
             Env1};
        {type_error, _Type} ->
            throw(type_error(pattern, P, Tuple, Ty))
    end;
add_type_pat(Atom = {atom, P, Val}, Ty, Env) ->
    LitTy = {atom, erl_anno:new(0), Val},
    case subtype(LitTy, Ty, Env) of
        true ->
            {LitTy, LitTy, Env};
        false ->
            throw(type_error(pattern, P, Atom, Ty))
    end;
add_type_pat(Nil = {nil, P}, Ty, Env) ->
    NilTy = type(nil),
    case subtype(NilTy, Ty, Env) of
        true ->
            {NilTy, NilTy, Env};
        false ->
            throw(type_error(pattern, P, Nil, Ty))
    end;
add_type_pat(CONS = {cons, P, PH, PT}, ListTy, Env) ->
    case expect_list_type(normalize(ListTy, Env), dont_allow_nil_type, Env) of
        any ->
            Env2 = add_any_types_pat(PH, Env),
            TailTy = normalize(type(union, [ListTy, type(nil)]), Env),
            {_TailPatTy, _TauUBound, VEnv3} = add_type_pat(PT, TailTy, Env2),
            NonEmptyTy = rewrite_list_to_nonempty_list(ListTy),
            {type(none), NonEmptyTy, VEnv3};
        {elem_ty, ElemTy} ->
            {PatTy1, _UBound1, Env2} =
                add_type_pat(PH, normalize(ElemTy, Env), Env),
            TailTy = normalize(type(union, [ListTy, type(nil)]), Env),
            {_PatTy2, _Ubound2, Env3} = add_type_pat(PT, TailTy, Env2),
            {PatTy, Env4} = case should_list_pat_enable_exhaustiveness_check(CONS) of
                                true ->
                                    {type(nonempty_list, [PatTy1]), Env3};
                                false ->
                                    {type(none), disable_exhaustiveness_check(Env3)}
                            end,
            NonEmptyTy = rewrite_list_to_nonempty_list(ListTy),
            {PatTy, NonEmptyTy, Env4};
        {type_error, _Ty} ->
            throw(type_error(cons_pat, P, CONS, ListTy))
    end;
add_type_pat(String = {string, P, _}, Ty, Env) ->
    case subtype(type(string), Ty, Env) of
        true ->
            {type(none), normalize(type(string), Env), Env};
        false ->
            throw(type_error(pattern, P, String, Ty))
    end;
add_type_pat({bin, _P, BinElements} = Bin, Ty, Env) ->
    %% Check the size parameters of the bit pattern
    BinTy = gradualizer_bin:compute_type(Bin),
    case subtype(BinTy, Ty, Env) of
        true ->
            ok;
        false ->
            throw(type_error(Bin, BinTy, Ty))
    end,
    %% Check the elements
    Env2 =
        lists:foldl(fun ({bin_element, _, Pat, _Size, _Specifiers} = BinElem,
                         EnvAcc) ->
                            %% Check Pat against the bit syntax type specifiers
                            ElemTy = type_of_bin_element(BinElem, pattern),
                            {_PatTy, _UBound, Env2} = add_type_pat(Pat, ElemTy, EnvAcc),
                            Env2
                    end,
                    Env,
                    BinElements),
    {PatTy, Env3} = case should_bin_pat_enable_exhaustiveness_check(BinElements) of
                        true ->
                            {BinTy, Env2};
                        false ->
                            {type(none), disable_exhaustiveness_check(Env2)}
                    end,
    {PatTy, BinTy, Env3};
add_type_pat({record, P, Record, Fields}, Ty, Env) ->
    case expect_record_type(Ty, Record, Env) of
        any ->
            NewEnv = union_var_binds([Env] ++ [ add_any_types_pat(Field, Env)
                                                || ?record_field_expr(Field) <- Fields ], Env),
            {type(none), Ty, NewEnv};
        {fields_ty, Tys} ->
            {PatTys, UBounds, Env1} = add_type_pat_fields(Fields, Tys, Env),
            {type_record(Record, PatTys),
             type_record(Record, UBounds),
             Env1};
        {type_error, _Type} ->
            throw(type_error(record_pattern, P, Record, Ty))
    end;
add_type_pat({map, P, AssocPats} = MapPat, MapTy, Env) ->
    NormMapTy = normalize(MapTy, Env),
    case expect_map_type(NormMapTy, Env) of
        any ->
            NewEnv = add_any_types_pat(MapPat, Env),
            {type(none), type(any), NewEnv};
        {assoc_tys, AssocTys} ->
            AssocTys = ?assert_type(AssocTys, [gradualizer_type:af_assoc_type()]),
            %% Check each Key := Value and bind vars in Value.
            CheckAssoc = fun ({map_field_exact, _, Key, ValuePat},
                              {EnvIn, ExhaustedAssocsAcc, RemainingAssocsIn}) ->
                                 case add_type_pat_map_key(Key, RemainingAssocsIn, EnvIn, []) of
                                     {ok, KeyPatTy, ValueTy, RemainingAssocsOut} ->
                                         {ValPatTy, _ValUBound, EnvOut} =
                                             add_type_pat(ValuePat, normalize(ValueTy, EnvIn), EnvIn),
                                         ExhaustedAssoc = type_assoc(map_field_exact,
                                                               [KeyPatTy, ValPatTy]),
                                         {EnvOut,
                                          [ExhaustedAssoc | ExhaustedAssocsAcc],
                                          RemainingAssocsOut};
                                     error ->
                                         %% TODO: allow patterns like #{x := a, x := a}
                                         throw(type_error(badkey, Key, MapTy))
                                 end
                         end,
            {NewEnv, ExhaustedAssocsRev, RemainingAssocs} =
                lists:foldl(CheckAssoc, {Env, [], AssocTys}, AssocPats),
            PatTy = case NormMapTy of
                        ?top() ->
                            top();
                        {var, _, _Var} ->
                            type(none);
                        ?type(map, Assocs) when is_list(Assocs) ->
                            ExhaustedAssocs = lists:reverse(ExhaustedAssocsRev),
                            AllExhaustedAssocs = case RemainingAssocs of
                                any -> ExhaustedAssocs;
                                _List -> ExhaustedAssocs ++ RemainingAssocs
                            end,
                            PatTy0 = type_map(AllExhaustedAssocs),
                            handle_possible_none_map_keys(PatTy0)
                    end,
            {PatTy, MapTy, NewEnv};
        {type_error, _Type} ->
            throw(type_error(pattern, P, MapPat, MapTy))
    end;
add_type_pat({match, _, {var, _, _Var} = PatVar, Pat}, Ty, Env) ->
    add_type_pat_var(Pat, PatVar, Ty, Env);
add_type_pat({match, _, Pat, {var, _, _Var} = PatVar}, Ty, Env) ->
    add_type_pat_var(Pat, PatVar, Ty, Env);
add_type_pat({match, _, Pat1, Pat2}, Ty, Env) ->
    %% Use the refined type of Pat2 to bind vars in Pat1.
    {PatTy1, Ty1, Env1} = add_type_pat(Pat2, Ty, Env),
    {PatTy2, Ty2, Env2} = add_type_pat(Pat1, Ty1, Env1),
    GlbTy = glb(PatTy1, PatTy2, Env),
    {GlbTy, Ty2, Env2};
add_type_pat({op, _, '++', Pat1, Pat2}, Ty, Env) ->
    {_, _, Env1} = add_type_pat(Pat1, Ty, Env),
    {_, _, Env2} = add_type_pat(Pat2, Ty, Env1),
    {type(none), Ty, Env2};
add_type_pat(OpPat = {op, _Anno, _Op, _Pat1, _Pat2}, Ty, Env) ->
    %% Operator patterns are evaluated at compile-time by the compiler.
    %% So we simply evaluate them and check the type of the resulting value.
    %% This simplified situations like this: suppose Ty = non_neg_integer() and
    %% the pattern is 12 - 13. Is this pattern of the correct type? It is not
    %% enough to just check the two arguments to the operator (which would say that
    %% the pattern has the correct type) but we also need to check that the left
    %% argument is not smaller than the right argument. But instead of implementing
    %% such a check, we simply evaluate the pattern as an expression.
    add_type_pat_literal(OpPat, Ty, Env);
add_type_pat(OpPat = {op, _Anno, _Op, _Pat}, Ty, Env) ->
    add_type_pat_literal(OpPat, Ty, Env);
add_type_pat(Pat, Ty, _Env) ->
    throw(type_error(pattern, element(2, Pat), Pat, Ty)).

-spec add_type_pat_union(pattern(), type(), env()) -> R when
      R :: {PatTy :: type(),
            UBound :: type(),
            NewEnv :: env()}.
add_type_pat_union(Pat, ?type(union, UnionTys) = UnionTy, Env) ->
    {PatTys, UBounds, Envs} =
        lists:foldr(fun (Ty, {PatTysAcc, UBoundsAcc, EnvAcc} = Acc) ->
                        try do_add_types_pats([Pat], [Ty], Env) of
                            {[PatTy], [UBound], NewEnv} ->
                                {[PatTy|PatTysAcc],
                                 [UBound|UBoundsAcc],
                                 [NewEnv|EnvAcc]}
                        catch _TypeError ->
                            Acc
                        end
                    end,
                    {[], [], []},
                    UnionTys),
    case PatTys of
        [] ->
            %% Pattern doesn't match any type in the union
            Anno = element(2, Pat),
            throw(type_error(pattern, Anno, Pat, UnionTy));
        _SomeTysMatched ->
            %% TODO: The var binds should be merged with the intersection semantics.
            %% TODO by erszcz: see tuple_union_arg:j/1 for a problem with this.
            %% To solve this we might need to erase var binds gathered in the member patterns and
            %% instead bind the vars to fresh type vars.
            %% The type vars would have upper bounds of LUB(member var binds' types).
            %% This is food for thought, it might or might not work.
            %% erszcz: I'm not sure why var binds with intersection.
            %%         LUB on var binds seems to solve some self-check issues
            %%         (and generate more, but valid ones).
            {lub(PatTys, Env),
             normalize(type(union, UBounds), Env),
             union_var_binds_symmetrical(Envs, Env)}
    end.

%% TODO: This is incomplete!
%% To properly check pattern exhaustiveness we have to consider bound variables.
%%
%% See also `are_patterns_matching_all_input/2',
%% which is similar, but limited to variable patterns.
-spec should_list_pat_enable_exhaustiveness_check(expr()) -> boolean().
should_list_pat_enable_exhaustiveness_check({nil, _}) -> true;
should_list_pat_enable_exhaustiveness_check({cons, _, {var, _, _}, {var, _, _}}) -> true;
should_list_pat_enable_exhaustiveness_check(_) -> false.

%% TODO: This is incomplete!
%% To properly check pattern exhaustiveness we have to consider bound variables.
%% Pattern: `<<>>'
-spec should_bin_pat_enable_exhaustiveness_check(list()) -> boolean().
should_bin_pat_enable_exhaustiveness_check([]) ->
    true;
%% Pattern: `<<_, _/bytes>>'
should_bin_pat_enable_exhaustiveness_check([{bin_element, _, {var, _, _}, _, _},
                       {bin_element, _, {var, _, _}, _, [Bytes]}])
  when Bytes =:= bytes; Bytes =:= binary ->
    true;
should_bin_pat_enable_exhaustiveness_check(_) ->
    false.

-spec expect_map_type(type(), env()) -> R when
      R :: any
         | {assoc_tys, [type()] | any}
         | {type_error, type()}.
expect_map_type(?type(any), _Env) ->
    any;
expect_map_type(?top(), _Env) ->
    {assoc_tys, [type(map_field_assoc, [top(), top()])]};
expect_map_type(?type(map, AssocTys), _Env) ->
    {assoc_tys, AssocTys};
expect_map_type(Ty, _Env) ->
    {type_error, Ty}.

-spec add_type_pat_var(_, _, type(), env()) -> any().
add_type_pat_var(Pat, PatVar, Ty, Env) ->
    %% Refine using Pat1 first to be able to bind Pat2 to a refined type.
    {PatTy1, Ty1, Env1} = add_type_pat(Pat, Ty, Env),
    {PatTy2, Ty2, Env2} = add_type_pat(PatVar, Ty1, Env1),
    GlbTy = glb(PatTy1, PatTy2, Env),
    {GlbTy, Ty2, Env2}.

-spec add_type_pat_literal(_, _, env()) -> any().
add_type_pat_literal(Pat, Ty, Env) ->
    case erl_eval:partial_eval(Pat) of
        Literal when Literal =/= Pat ->
            %% If Pat is returned, it's an illegal, non-constant pattern that
            %% should not appear in compilable code, i.e. caught by linter.
            try
                add_type_pat(Literal, Ty, Env)
            catch
                _TypeError ->
                    %% throw with the original pattern
                    throw(type_error(operator_pattern, Pat, Ty))
            end
    end.

-spec find_field_or_create([record_field()], atom(), expr()) -> record_field().
find_field_or_create([], Name, Default) -> {record_field, erl_anno:new(0), {atom, erl_anno:new(0), Name}, Default};
find_field_or_create([Field = ?record_field(Name)|_], Name, _Default) -> Field;
find_field_or_create([_|Fields], Name, Default) -> find_field_or_create(Fields, Name, Default).

-spec find_field_default([record_field()]) -> expr().
find_field_default([]) -> {var, erl_anno:new(0), '_'};
find_field_default([{record_field, _, {var, _, '_'}, Exp} | _]) -> Exp;
find_field_default([_ | Fields]) -> find_field_default(Fields).

-spec add_type_pat_fields(_, _, env()) -> any().
add_type_pat_fields([], _, Env) ->
    {[], [], Env};
add_type_pat_fields(Fields, Tys, Env) ->
    %% Add every missing fields
    %% If an underscore field is present: use that expression as the default expression
    %% Otherwise, give an empty assignment of the field to underscore
    Default = find_field_default(Fields),
    AllFields = [ find_field_or_create(Fields, Name, Default) || ?typed_record_field(Name) <- Tys],
    add_type_pat_fields(AllFields, Tys, Env, [], []).

-spec add_type_pat_fields(_, _, env(), _, _) -> any().
add_type_pat_fields([], _, Env, PatTysAcc, UBoundsAcc) ->
    {lists:reverse(PatTysAcc), lists:reverse(UBoundsAcc), Env};
add_type_pat_fields([{record_field, _, {atom, _, Name} = FieldWithAnno, Pat}|Fields],
                    Record, Env, PatTysAcc, UBoundsAcc) ->
    Ty = get_rec_field_type(FieldWithAnno, Record),
    NormTy = normalize(Ty, Env),
    {PatTyNorm, UBoundNorm, Env2} =
        ?throw_orig_type(add_type_pat(Pat, NormTy, Env), Ty, NormTy),
    %% De-normalize the returned types if they are the type checked against.
    RawPatTy  = case PatTyNorm  of NormTy -> Ty;
                                _      -> PatTyNorm end,
    PatTy = type_field_type(Name, RawPatTy),
    RawUBound = case UBoundNorm of NormTy -> Ty;
                                _      -> UBoundNorm end,
    UBound = type_field_type(Name, RawUBound),
    add_type_pat_fields(Fields, Record, Env2, [PatTy|PatTysAcc], [UBound|UBoundsAcc]).

%% Given a pattern for a key, finds the matching association in the map type and
%% returns the value type. Returns 'error' if the key is not valid in the map.
-spec add_type_pat_map_key(Key         :: gradualizer_type:abstract_pattern(),
                           MapTyAssocs :: any | [af_assoc_type()],
                           Env         :: env(),
                           PrevAssocs  :: [af_assoc_type()]
                          ) -> {ok,
                                KeyPatTy :: type(),
                                ValueTy :: type(),
                                RemainingAssocs :: any | [af_assoc_type()]} |
                               error.
add_type_pat_map_key(_Key, any, _Env, _PrevAssocs) ->
    {ok, type(none), type(any), any};
add_type_pat_map_key(Key, [{type, _, AssocTag, [KeyTy, ValueTy]} = Assoc | NextAssocs],
                     Env, PrevAssocs)
  when AssocTag == map_field_exact; AssocTag == map_field_assoc ->
    try add_types_pats([Key], [KeyTy], Env, capture_vars) of
        {[KeyPatTy], _KeyUBound, _VEnv} ->
            %% No free vars in Key, so no new variable binds.  (Types in VEnv
            %% can be refined though, so _VEnv doesn't have to match VEenv.)

            %% Remove KeyPatTy from the assoc or remove it completely.
            %% Motivation. If we have the type #{x|y := a} then the pattern
            %% #{x := a} matches the value #{x := a} but also #{x := a, y := a}
            %% as the all instances of the key type can appear together as keys
            %% of the map. Thus after matching x := a, we have to keep y => a.
            MaybeRemainingAssoc = case type_diff(KeyTy, KeyPatTy, Env) of
                %% If KeyPatTy covers KeyTy completely, remove the assoc.
                ?type(none) -> [];
                %% The remaining association is always map_field_assoc as one key
                %% of that type has just been found, and that is enough to satisfy
                %% the requirement of map_field_exact.
                RemKeyTy -> [type_assoc(map_field_assoc, [RemKeyTy, ValueTy])]
            end,
            UpdatedTail = MaybeRemainingAssoc ++ NextAssocs,
            UpdatedAssocs = lists:reverse(PrevAssocs) ++ UpdatedTail,

            {ok, KeyPatTy, ValueTy, UpdatedAssocs}
    catch _TypeError ->
        add_type_pat_map_key(Key, NextAssocs, Env, [Assoc | PrevAssocs])
    end;
add_type_pat_map_key(_Key, [], _Env, _PrevAssocs) ->
    %% Key is not defined in this map type.
    error.

%% If any of the key types is none(), replace the map type with none().
%% This can happen because of imprecision. E.g., when a pattern #{"abc" := abc}
%% is used to match against the type #{string() => atom()}.
-spec handle_possible_none_map_keys(gradualizer_type:af_map_type()) -> type().
handle_possible_none_map_keys(?type(map, any) = Ty) ->
    Ty;
handle_possible_none_map_keys(?type(map, Assocs) = Ty) when Assocs /= any ->
    case lists:any(fun(?type(_Tag, [?type(none), _Value])) -> true;
                      (_) -> false
                   end, Assocs) of
        true -> type(none);
        false -> Ty
    end.

-spec add_any_types_pats([gradualizer_type:abstract_pattern()], Env :: env()) ->
                             NewEnv :: env().
add_any_types_pats([], Env) ->
    Env;
add_any_types_pats([Pat|Pats], Env) ->
    add_any_types_pats(Pats, add_any_types_pat(Pat, Env)).

-spec add_any_types_pat(gradualizer_type:abstract_pattern(), Env :: env()) ->
                            NewEnv :: env().
add_any_types_pat({atom, _, _}, Env) ->
    Env;
add_any_types_pat({integer, _, _}, Env) ->
    Env;
add_any_types_pat({char, _, _}, Env) ->
    Env;
add_any_types_pat({float, _, _}, Env) ->
    Env;
add_any_types_pat({match, _, P1, P2}, Env) ->
    add_any_types_pats([P1, P2], Env);
add_any_types_pat({cons, _, Head, Tail}, Env) ->
    add_any_types_pats([Head, Tail], Env);
add_any_types_pat({string, _, _}, Env) ->
    Env;
add_any_types_pat({nil, _}, Env) ->
    Env;
add_any_types_pat({tuple, _, Pats}, Env) ->
    add_any_types_pats(Pats, Env);
add_any_types_pat({record, _, _RecName, Fields}, Env) ->
    add_any_types_pats(
      [Value || {record_field, _, _Name, Value} <- Fields], Env);
add_any_types_pat({map, _, Fields}, Env) ->
    add_any_types_pats(
      lists:flatmap(
        fun({Tag, _, Name, Value})
              when Tag =:= map_field_assoc; Tag =:= map_field_exact ->
                [Name, Value]
        end, Fields),
      Env);
add_any_types_pat({bin, _, BinElements}, Env) ->
    lists:foldl(fun ({bin_element, _, Pat, _Size, _Specifiers}, Env1) ->
                        %% TODO: Ideally, we should use the bit specifiers to
                        %% add types to the variables in Pat. But we don't
                        %% have access to the TEnv here, so we cannot call
                        %% add_type_pat. Perhaps we should change that.
                        add_any_types_pat(Pat, Env1)
                    end,
                    Env,
                    BinElements);
add_any_types_pat({var, _, '_'}, Env) ->
    Env;
add_any_types_pat({var, _, A}, Env) ->
    VEnv = Env#env.venv,
    Env#env{venv = VEnv#{ A => type(any) }};
add_any_types_pat({op, _, '++', _Pat1, Pat2}, Env) ->
    %% Pat1 cannot contain any variables so there is no need to traverse it.
    add_any_types_pat(Pat2, Env);
add_any_types_pat({op, _, _Op, _Pat1, _Pat2}, Env) ->
    %% These patterns cannot contain variables.
    Env;
add_any_types_pat({op, _, _Op, _Pat}, Env) ->
    %% Cannot contain variables.
    Env;
add_any_types_pat(Pat, _Env) ->
    %% Matching other patterns and throwing (but NOT erroring) simplifies property based testing.
    throw(illegal_pattern(Pat)).

%% Assigns the type top() to variables occurring more than once in the list of
%% patterns. This prevents the variables from being treated as match-all, thus
%% affecting refinement.
-spec assign_types_to_vars_bound_more_than_once(Pats, Env, Caps) -> R when
      Pats :: [gradualizer_type:abstract_pattern()],
      Env  :: env(),
      Caps :: capture_vars | bind_vars,
      R    :: env().
assign_types_to_vars_bound_more_than_once(Pats, Env, Caps) ->
    VEnv = Env#env.venv,
    VarOccurrences = count_var_occurrences(Pats),
    Fun = case Caps of
              capture_vars -> fun assign_types_helper_capture_vars/3;
              bind_vars    -> fun assign_types_helper_bind_vars/3
          end,
    Env#env{venv = maps:fold(Fun, VEnv, VarOccurrences)}.

-spec assign_types_helper_capture_vars(Var :: atom(), N :: pos_integer(), VEnvAcc :: map()) -> map().
assign_types_helper_capture_vars(Var, N, VEnvAcc) when N > 1 ->
    case maps:is_key(Var, VEnvAcc) of
        true  -> VEnvAcc;
        false -> VEnvAcc#{Var => top()}
    end;
assign_types_helper_capture_vars(_Var, 1, VEnvAcc) ->
    %% Leave it as a free variable
    VEnvAcc.

-spec assign_types_helper_bind_vars(Var :: atom(), N :: pos_integer(), VEnvAcc :: map()) -> map().
assign_types_helper_bind_vars(Var, N, VEnvAcc) when N > 1 ->
    %% Overwrite with top(), which is refined later
    VEnvAcc#{Var => top()};
assign_types_helper_bind_vars(Var, 1, VEnvAcc) ->
    %% Free the variable by removing it from the environment
    maps:remove(Var, VEnvAcc).

%% Counts the variables in a list of expressions or patterns.
-spec count_var_occurrences([gradualizer_type:abstract_expr()]) ->
                                   #{atom() => pos_integer()}.
count_var_occurrences(Exprs) ->
    Inc = fun (N) -> N + 1 end,
    gradualizer_lib:fold_ast(fun ({var, _, Var}, Acc) when Var =/= '_' ->
                                     maps:update_with(Var, Inc, 1, Acc);
                                 (_, Acc) ->
                                     Acc
                             end,
                             #{},
                             Exprs).

%% Get type from specifiers in a bit syntax, e.g. <<Foo/float-little>>
-spec type_of_bin_element({bin_element,
                           Anno       :: erl_anno:anno(),
                           Expr       :: gradualizer_type:abstract_expr(),
                           Size       :: non_neg_integer() |
                                         default,
                           Specifiers :: [atom() | {unit, pos_integer()}] |
                                         default},
                           OccursAs   :: pattern | expr) -> type().
type_of_bin_element({bin_element, Anno, Expr, Size, default}, OccursAs) ->
    type_of_bin_element({bin_element, Anno, Expr, Size, []}, OccursAs);
type_of_bin_element({bin_element, _P, Expr, _Size, Specifiers}, OccursAs) ->
    %% String literal is syntactic sugar for multiple char literals,
    IsStringLiteral = case Expr of
                          {string, _, _} -> true;
                          _              -> false
                      end,
    IsSigned =
        case OccursAs of
            pattern -> lists:member(signed, Specifiers);
            expr -> true
        end,
    Types =
        lists:filtermap(fun
                            (S) when S == integer;
                                     S == utf8;
                                     S == utf16;
                                     S == utf32 ->
                                if
                                    IsStringLiteral ->
                                        %% <<"ab"/utf8>> == <<$a/utf8, $b/utf8>>.
                                        {true, type(string)};
                                    IsSigned ->
                                        {true, type(integer)};
                                    true ->
                                        {true, type(non_neg_integer)}
                                end;
                            (float) when IsStringLiteral ->
                                %% <<"abc"/float>> is integers to floats conversion
                                {true, type(string)};
                            (float) ->
                                %% Integers can be cast to floats in this way.
                                {true, type(number)};
                            (S) when S == binary; S == bytes ->
                                %% TODO: Consider Size and Unit
                                Ty = ?assert_type({type, erl_anno:new(0), binary,
                                                   [{integer, erl_anno:new(0), 0},
                                                    {integer, erl_anno:new(0), 8}]},
                                                  type()),
                                {true, Ty};
                            (S) when S == bitstring; S == bits ->
                                %% TODO: Consider Size and Unit
                                Ty = ?assert_type({type, erl_anno:new(0), binary,
                                                   [{integer, erl_anno:new(0), 0},
                                                    {integer, erl_anno:new(0), 1}]},
                                                  type()),
                                {true, Ty};
                            (_NotATypeSpecifier) ->
                                false
                        end,
                        Specifiers),
    case Types of
        [] when IsStringLiteral ->
            %% <<"abc">>
            type(string);
        [] when IsSigned ->
            %% As expr: <<X>>
            %% As pattern: <<X/signed>>
            type(integer);
        [] when not IsSigned ->
            %% As pattern: <<X>> or <<X/unsigned>>
            type(non_neg_integer);
        [T] ->
            %% We can assert because in the listmap we always return either false or {true, type()}.
            ?assert_type(T, type())
    end.


%%% Helper functions

-spec type(_, _) -> type().
type(Name, Args) ->
    {type, erl_anno:new(0), Name, Args}.

%% Helper to create a type, typically a normalized type
-spec type(atom()) -> type().
type(top) ->
    top();
type(list) ->
    type(list, [type(any)]);
type(bitstring) ->
    type(binary, [{integer, erl_anno:new(0), 0},
                  {integer, erl_anno:new(0), 1}]);
type(binary) ->
    type(binary, [{integer, erl_anno:new(0), 0},
                  {integer, erl_anno:new(0), 8}]);
type('fun') ->
    %% fun() is normalized as fun((...) -> any())
    type('fun', [{type, erl_anno:new(0), any}, type(any)]);
type(Name) ->
    type(Name, []).

-spec singleton(any(), any()) -> type().
singleton(atom, A) when is_atom(A) -> {atom, erl_anno:new(0), A};
singleton(char, C) when is_integer(C), C >= 0, C =< 16#10ffff -> {char, erl_anno:new(0), C};
singleton(integer, I) when is_integer(I) -> {integer, erl_anno:new(0), I}.

-spec top() -> type().
top() ->
    {remote_type, erl_anno:new(0), [{atom, erl_anno:new(0), gradualizer}
				   ,{atom, erl_anno:new(0), top},[]]}.

-spec type_record(atom()) -> type().
type_record(Name) ->
    type_record(Name, []).

-spec type_record(atom(), list()) -> type().
type_record(Name, Fields) ->
    {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Name} | Fields]}.

-spec type_field_type(atom(), type()) -> record_field_type().
type_field_type(Name, Type) ->
    {type, erl_anno:new(0), field_type, [{atom, erl_anno:new(0), Name}, Type]}.

-spec type_map(any | [af_assoc_type()]) -> gradualizer_type:af_map_type().
type_map(Assocs) ->
    ?assert_type(type(map, Assocs), gradualizer_type:af_map_type()).

-spec type_assoc(map_field_assoc | map_field_exact, [type()]) -> af_assoc_type().
type_assoc(AssocKind, KeyAndValueTy) ->
    % af_assoc_type() is defined using a top-level union,
    % and Gradualizer sees it as a different thing than
    % the inferred type that has an inner union instead:
    % {type, anno(), map_field_assoc | map_field_exact, [type()]}
    ?assert_type({type, erl_anno:new(0), AssocKind, KeyAndValueTy},
                 af_assoc_type()).

-spec type_fun([type()], type()) -> type().
type_fun(Args, ResTy) ->
    type('fun', [type(product, Args), ResTy]).

-spec type_fun(arity()) -> type().
type_fun(Arity) ->
    Args = [type(any) || _ <- lists:seq(1, Arity)],
    type_fun(Args, type(any)).

-spec type_atom(atom()) -> type().
type_atom(Name) ->
    {atom, erl_anno:new(0), Name}.

-spec is_power_of_two(_) -> boolean().
is_power_of_two(0) -> false;
is_power_of_two(1) -> true;
is_power_of_two(N) when N rem 2 == 0 ->
    is_power_of_two(N div 2);
is_power_of_two(_) -> false.

-spec union_var_binds_symmetrical([env()], env()) -> env().
union_var_binds_symmetrical([], Env) ->
    Env;
union_var_binds_symmetrical(Envs, #env{} = Env) ->
    Lub = fun(_K, Ty1, Ty2) ->
                  UTy = type(union, [Ty1, Ty2]),
                  NTy = normalize(UTy, Env),
                  NTy
          end,
    Env#env{venv = union_var_binds_symmetrical_help([ E#env.venv || E <- Envs ], Lub)}.

-spec union_var_binds_symmetrical_help([venv()], function()) -> venv().
union_var_binds_symmetrical_help([VB1, VB2 | Rest], Lub) ->
    VB =
        case maps:size(VB1) < maps:size(VB2) of
            true ->
                maps:fold(fun (K, Ty1, VB) ->
                    case maps:is_key(K, VB2) of
                        false -> VB;
                        true ->
                            Ty2 = maps:get(K, VB2),
                            Ty = Lub(K, Ty1, Ty2),
                            maps:put(K, Ty, VB)
                    end
                end, #{}, VB1);
            false ->
                maps:fold(fun (K, Ty2, VB) ->
                    case maps:is_key(K, VB1) of
                        false -> VB;
                        true ->
                            Ty1 = maps:get(K, VB1),
                            Ty = Lub(K, Ty1, Ty2),
                            maps:put(K, Ty, VB)
                    end
                end, #{}, VB2)
        end,
    union_var_binds_symmetrical_help([VB | Rest], Lub);
union_var_binds_symmetrical_help([VB], _) ->  VB.

-spec union_var_binds(env(), env(), env()) -> env().
union_var_binds(#env{} = Env1, #env{} = Env2, #env{} = Env) ->
    union_var_binds([Env1, Env2], Env).

%% This function has been identified as a bottleneck.
%% Without tail recursion, the gradualizer would hang when self-gradualizing
%% when called from add_type_pat/4, the clause where the type is a union type.
-spec union_var_binds([env()], env()) -> env().
union_var_binds([], #env{} = Env) ->
    Env;
union_var_binds(Envs, #env{} = Env) ->
    Glb = fun(_K, Ty1, Ty2) -> glb(Ty1, Ty2, Env) end,
    Env#env{venv = union_var_binds_help([ E#env.venv || E <- Envs ], Glb)}.

%% Tail recursive helper.
-spec union_var_binds_help([venv()], _) -> venv().
union_var_binds_help([#{} = VB1, #{} = VB2 | Rest], Glb) ->
    VB = gradualizer_lib:merge_with(Glb, VB1, VB2),
    union_var_binds_help([VB | Rest], Glb);
union_var_binds_help([#{} = VB], _) -> VB.

-spec add_var_binds(env(), env(), env()) -> env().
add_var_binds(#env{venv = VB1}, #env{venv = VB2}, #env{} = Env) ->
    Glb = fun(_K, Ty1, Ty2) -> glb(Ty1, Ty2, Env) end,
    Env#env{venv = gradualizer_lib:merge_with(Glb, VB1, VB2)}.

%% Set the type of a new variable.
-spec set_var_type(env(), {var, _, atom()}, type()) -> env().
set_var_type(Env, {var, _, A} = V, Ty) ->
    ?verbose(Env, "~sSetting var type ~s :: ~s~n",
             [gradualizer_fmt:format_location(V, brief), A, typelib:pp_type(Ty)]),
    VEnv = Env#env.venv,
    Env#env{venv = VEnv#{A => Ty}}.

%% Update the type of an already seen variable.
-spec update_var_type(env(), {var, _, atom()}, type()) -> env().
update_var_type(Env, {var, _, A} = V, Ty) ->
    ?verbose(Env, "~sUpdating var type ~s :: ~s~n",
             [gradualizer_fmt:format_location(V, brief), A, typelib:pp_type(Ty)]),
    VEnv = Env#env.venv,
    Env#env{venv = VEnv#{A := Ty}}.

%% From a record field name, find the default value from the typed list of record field definitions
-spec get_rec_field_default(af_field_name(), [typed_record_field()]) -> expr().
get_rec_field_default({atom, _, FieldName},
                    [{typed_record_field,
                        {record_field, _, {atom, _, FieldName}, Default}, _}|_]) ->
    Default;
get_rec_field_default(FieldWithAnno, [_|RecFields]) ->
    get_rec_field_default(FieldWithAnno, RecFields);
get_rec_field_default(FieldWithAnno, []) ->
    throw(undef(record_field, FieldWithAnno)).

-spec get_rec_field_type(af_field_name(), [typed_record_field()]) -> type().
get_rec_field_type(FieldWithAnno, RecFields) ->
    %% The first field is the second element of the tuple - so start from 2
    {_Index, Ty} = get_rec_field_index_and_type(FieldWithAnno, RecFields, 2),
    Ty.

-spec get_rec_field_type(af_field_name(), FieldTypes, FieldTypes) -> R when
      FieldTypes :: [{atom(), type()}],
      R :: type().
get_rec_field_type(FieldWithAnno, FieldTypes1, FieldTypes2) ->
    {_, _, Name} = FieldWithAnno,
    case lists:keyfind(Name, 1, FieldTypes1) of
        {Name, Ty} -> Ty;
        false ->
            case lists:keyfind(Name, 1, FieldTypes2) of
                {Name, Ty} -> Ty;
                false ->
                    throw(undef(record_field, FieldWithAnno))
            end
    end.

-spec get_rec_field_index(af_field_name(), [typed_record_field()]) -> pos_integer().
get_rec_field_index(FieldWithAnno, RecFields) ->
    %% The first field is the second element of the tuple - so start from 2
    {Index, _Ty} = get_rec_field_index_and_type(FieldWithAnno, RecFields, 2),
    Index.

-spec get_rec_field_index_and_type(af_field_name(), [typed_record_field()], pos_integer()) -> R when
      R :: {pos_integer(), type()}.
get_rec_field_index_and_type({atom, _, FieldName}, [?typed_record_field(FieldName, Ty) | _], I) ->
    {I, Ty};
get_rec_field_index_and_type(FieldWithAnno, [_|RecFieldTypes], I) ->
    get_rec_field_index_and_type(FieldWithAnno, RecFieldTypes, I + 1);
get_rec_field_index_and_type(FieldWithAnno, [], _) ->
    throw(undef(record_field, FieldWithAnno)).

%% Helper for finding the return type of record_info/2
-spec get_record_info_type(gradualizer_type:abstract_expr(), env()) -> type().
get_record_info_type({call, Anno, {atom, _, record_info},
                      [{atom, _, fields}, {atom, _, RecName}]}, Env) ->
    Fields = get_record_fields(RecName, Anno, Env),
    Names = [Name
             || {typed_record_field, {record_field, _, Name, _}, _Ty} <- Fields],
    type(list, [type(union, Names)]);
get_record_info_type({call, Anno, {atom, _, record_info},
                      [{atom, _, size}, {atom, _, RecName}]}, Env) ->
    Fields = get_record_fields(RecName, Anno, Env),
    {integer, erl_anno:new(0), length(Fields) + 1}.

-spec record_field_types(list()) -> [{atom(), type()}].
record_field_types(Fields) ->
    lists:map(fun
                  (?type_field_type(Name, Type)) ->
                      {Name, Type};
                  (?typed_record_field(Name, Type)) ->
                      {Name, Type}
              end, Fields).

% Helper returning the type of module_info/0 and module_info/1 functions
-spec get_module_info_type(arity()) -> type() | [type()].
get_module_info_type(0) ->
    Options = get_module_info_options(),
    DefaultKeys = [module, attributes, compile, exports, md5, native],
    DefaultOpts = lists:filter(fun ({Key, _}) -> lists:member(Key, DefaultKeys) end, Options),
    TupleTys = lists:map(fun ({Key, Ty}) -> type(tuple, [type_atom(Key), Ty]) end, DefaultOpts),
    type_fun([], type(list, [type(union, TupleTys)]));
get_module_info_type(1) ->
    Options = get_module_info_options(),
    lists:map(fun ({Key, Type}) -> type_fun([type_atom(Key)], Type) end, Options).

-spec get_module_info_options() -> [{atom(), type()}].
get_module_info_options() ->
    [
        {module, type(module)},
        {attributes, type(list, [type(tuple, [type(atom), type(any)])])},
        {compile, type(list, [type(tuple, [type(any), type(any)])])},
        {md5, type(binary)},
        {exports, type(list, [type(tuple, [type(atom), type(arity)])])},
        {functions, type(list, [type(tuple, [type(atom), type(arity)])])},
        {nifs, type(list, [type(tuple, [type(atom), type(arity)])])},
        {native, type(boolean)}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec type_check_forms(list(), proplists:proplist()) -> list().
type_check_forms(Forms, Opts) ->
    StopOnFirstError = proplists:get_bool(stop_on_first_error, Opts),
    ParseData = collect_specs_types_opaques_and_functions(Forms),
    Env = create_env(ParseData, Opts),
    ?verbose(Env, "Checking module ~p~n", [ParseData#parsedata.module]),
    AllErrors = lists:foldr(fun (Function, Errors) ->
                                    type_check_form_with_timeout(Function, Errors, StopOnFirstError, Env, Opts)
                            end, [], ParseData#parsedata.functions),
    lists:reverse(AllErrors).


%% @doc `type_check_form_with_timeout' is a workaround meant to improve usability in the
%% presence of possible infinite loops or bugs in the typechecker.
%%
%% When Gradualizer is run interactively or in the foreground,
%% detection of an infinite loop is unavoidable by the user seeing that the tool is stuck.
%%
%% However, when Gradualizer is run in the background (as an integration with editor / IDE)
%% we might not realise it's stuck in an infinite loop at all or do so only after the machine's fan
%% starts up. Moreover, in this case we'll not receive feedback from the tool.
%%
%% To avoid the user experience problem, it's better to opportunistically try performing the check,
%% but in the light of it taking too long, just forcibly break the infinite loop and report
%% a Gradualizer (NOT the checked program!) error.
-spec type_check_form_with_timeout(expr(), [any()], boolean(), env(), [any()]) -> [any()].
type_check_form_with_timeout(Function, Errors, StopOnFirstError, Env, Opts) ->
    %% TODO: make FormCheckTimeOut configurable
    FormCheckTimeOut = ?form_check_timeout_ms,
    ?verbose(Env, "Spawning async task...~n", []),
    Self = self(),
    Task = fun () ->
                   Self ! type_check_form(Function, Errors, StopOnFirstError,
                                          Env, Opts)
           end,
    {Pid, MRef} = spawn_monitor(Task),
    Result = receive
                 {crash, Crash, STrace} ->
                     ?verbose(Env, "Task reported crash on ~s~n",
                              [gradualizer_fmt:form_info(Function)]),
                     io:format("Crashing...~n"),
                     erlang:raise(throw, Crash, STrace),
                     [];
                 {error_trace, Error, Trace} ->
                     ?verbose(Env, "Task reported error with trace from ~s~n",
                              [gradualizer_fmt:form_info(Function)]),
                     erlang:raise(error, Error, Trace),
                     [];
                 {errors, Errors1} ->
                     ?verbose(Env, "Task returned from ~s with ~p~n",
                              [gradualizer_fmt:form_info(Function), Errors1]),
                     Errors1;
                 {'DOWN', MRef, _, _, Info} ->
                     ?verbose(Env, "Task crashed on form ~s~n",
                              [gradualizer_fmt:form_info(Function)]),
                     erlang:error(Info, [Function, Errors, StopOnFirstError, Env, Opts])
                 after FormCheckTimeOut ->
                     erlang:exit(Pid, kill),
                     ?verbose(Env, "Form check timeout on ~s~n",
                              [gradualizer_fmt:form_info(Function)]),
                     [{form_check_timeout, Function} | Errors]
             end,
    erlang:demonitor(MRef, [flush]),
    Result.

-spec type_check_form(_, [_], _, _, _) -> R when
      R :: {errors, list()}
         | {crash, any(), list()}
         | {error_trace, any(), list()}.
type_check_form(Function, Errors, StopOnFirstError, Env, Opts)
  when Errors =:= [];
       not StopOnFirstError ->
    CrashOnError = proplists:get_bool(crash_on_error, Opts),

    try type_check_function(Env, Function) of
        _VarBinds ->
            {errors, Errors}
    catch
        throw:Throw:STrace when CrashOnError ->
            {crash, Throw, STrace};
        throw:Throw ->
            {errors, [Throw | Errors]};
        error:Error:STrace ->
            %% A hack to hide the (very large) #env{} in
            %% error stacktraces. TODO: Add an opt for this.
            Trace = case STrace of
                        [{M, F, [#env{}|Args], Pos} | RestTrace] ->
                            [{M, F, ['*environment excluded*'|Args], Pos} | RestTrace];
                        Trace0 ->
                            Trace0
                    end,
            {error_trace, Error, Trace}
    end;
type_check_form(_Function, Errors, _StopOnFirstError, _Env, _Opts) ->
    {errors, Errors}.

-spec create_env(#parsedata{}, proplists:proplist()) -> env().
create_env(#parsedata{module    = Module
                     ,specs     = Specs
                     ,functions = Funs
                     ,types     = Types
                     ,opaques   = Opaques
                     ,records   = Records
                     ,imports   = Imports
                     }, Opts) ->
    FEnv = create_fenv(Specs, Funs),
    TEnv = gradualizer_lib:create_tenv(Module, Types ++ Opaques, Records),
    Imported = maps:from_list([{{F, A}, M} || {M, F, A} <- Imports]),
    #env{fenv = FEnv,
         tenv = TEnv,
         imported = Imported,
         %% Store some type checking options in the environment
         verbose = proplists:get_bool(verbose, Opts),
         union_size_limit = proplists:get_value(union_size_limit, Opts,
                                                default_union_size_limit()),
         solve_constraints = proplists:get_bool(solve_constraints, Opts),
         no_skip_complex_guards = proplists:get_bool(no_skip_complex_guards, Opts)}.

-spec default_union_size_limit() -> non_neg_integer().
default_union_size_limit() -> 30.

-spec create_fenv(list(), list()) -> map().
create_fenv(Specs, Funs) ->
% We're taking advantage of the fact that if a key occurs more than once
% in the list then it right-most occurrence will take precedence. In this
% case it will mean that if there is a spec, then that will take precedence
% over the default type any().
    maps:from_list(
      [ {{Name, NArgs}, [type(any)]}
        || {function,_, Name, NArgs, _Clauses} <- Funs
      ] ++
      [ {{Name, NArgs}, absform:normalize_function_type_list(Types)}
        || {{Name, NArgs}, Types} <- Specs
      ]
     ).

%% Collect the top level parse tree stuff returned by epp:parse_file/2.
-spec collect_specs_types_opaques_and_functions(forms()) -> #parsedata{}.
collect_specs_types_opaques_and_functions(Forms) ->
    aux(Forms, #parsedata{}).

%% Helper for collect_specs_types_opaques_and_functions/1
-spec aux(forms(), #parsedata{}) -> #parsedata{}.
aux([], Acc) ->
    Acc;
aux([Fun={function, _, _, _, _} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{functions = [Fun | Acc#parsedata.functions]});
aux([{attribute, _, module, Module} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{module = Module});
aux([{attribute, _, spec, Spec} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{specs = [Spec | Acc#parsedata.specs]});
aux([{attribute, _, type, Type} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{types = [Type | Acc#parsedata.types]});
aux([{attribute, _, opaque, Opaque} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{opaques = [Opaque | Acc#parsedata.opaques]});
aux([{attribute, _, record, Record} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{records = [Record | Acc#parsedata.records]});
aux([{attribute, _, export, Exports} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{exports = Exports ++ Acc#parsedata.exports});
aux([{attribute, _, import, {Module, Functions}} | Forms], Acc) ->
    MFAs = [{Module, Name, Arity} || {Name, Arity} <- Functions],
    aux(Forms, Acc#parsedata{imports = MFAs ++ Acc#parsedata.imports});
aux([{attribute, _, compile, CompileOpts} | Forms], Acc)
  when is_list(CompileOpts) ->
    Acc1 = lists:foldl(fun (export_all, AccAcc) ->
                               AccAcc#parsedata{export_all = true};
                           (_, AccAcc) ->
                               AccAcc
                       end,
                       Acc,
                       CompileOpts),
    aux(Forms, Acc1);
aux([{attribute, _, compile, export_all} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{export_all = true});
aux([_|Forms], Acc) ->
    aux(Forms, Acc).

%% Used by test module to cross-check number of reported errors
-spec number_of_exported_functions(forms()) -> non_neg_integer().
number_of_exported_functions(Forms) ->
    ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
    case ParseData#parsedata.export_all of
        true -> length(ParseData#parsedata.functions);
        false -> length(ParseData#parsedata.exports)
    end.

-spec type_error(type_error()) -> error().
type_error(Kind) ->
    {type_error, Kind}.

-spec type_error(type_error(), anno()) -> error().
type_error(Kind, P) ->
    {type_error, Kind, P}.

-spec type_error(expr(), type() | [type()], type()) -> error();
                (type_error(), anno() | type(), type() | expr()) -> error().
type_error(Kind, P, Ty) ->
    {type_error, Kind, P, Ty}.

-spec type_error(cyclic_type_vars, anno(), bounded_function(), list()) -> error();
                (type_error(), anno(), atom() | pattern(), type()) -> error();
                (type_error(), unary_op() | binary_op(), anno(), type()) -> error().
type_error(Kind, P, Info, Ty) ->
    {type_error, Kind, P, Info, Ty}.

-spec type_error(call_arity, anno(), atom(), arity(), arity()) -> error();
                (call_intersect, anno(), expr(), type(), [type()]) -> error();
                (type_error(), binary_op(), anno(), type(), type()) -> error().
type_error(Kind, Op, P, Ty1, Ty2) ->
    {type_error, Kind, Op, P, Ty1, Ty2}.

-spec undef(undef(), expr()) -> error().
undef(Kind, Info) ->
    {undef, Kind, Info}.

-spec undef(undef(), anno(), expr() | {atom(), arity() | atom()} | mfa()) -> error().
undef(Kind, P, Info) ->
    {undef, Kind, P, Info}.

-spec not_exported(remote_type, anno(), {module(), atom(), arity()}) -> error().
not_exported(Kind, P, Info) ->
    {not_exported, Kind, P, Info}.

-spec bad_type_annotation(gradualizer_type:af_string()) -> error().
bad_type_annotation(Info) ->
    {bad_type_annotation, Info}.

-spec illegal_map_type(type()) -> error().
illegal_map_type(Info) ->
    {illegal_map_type, Info}.

-spec argument_length_mismatch(anno(), arity(), arity()) -> error().
argument_length_mismatch(P, LenTy, LenArgs) ->
    {argument_length_mismatch, P, LenTy, LenArgs}.

-spec nonexhaustive(anno(), [expr()]) -> error().
nonexhaustive(P, Examples) ->
    {nonexhaustive, P, Examples}.

-spec illegal_pattern(pattern()) -> error().
illegal_pattern(Pat) ->
    {illegal_pattern, Pat}.

-spec internal_error(missing_type_spec, atom(), arity()) -> error().
internal_error(missing_type_spec, Name, NArgs) ->
    {internal_error, missing_type_spec, Name, NArgs}.

-spec call_undef(anno(), module(), atom(), arity()) -> error().
call_undef(P, Module, Name, Arity) ->
    {call_undef, P, Module, Name, Arity}.

-spec error_evidence(error()) -> any().
error_evidence({_, Evidence}) -> Evidence;
error_evidence({_, _, Evidence}) -> Evidence;
error_evidence({_, _, _, Evidence}) -> Evidence;
error_evidence({_, _, _, _, Evidence}) -> Evidence;
error_evidence({_, _, _, _, _, Evidence}) -> Evidence;
% the expected type (evidence) does not apper in constraint_error
error_evidence({constraint_error, _, _, _, _, _, _}) -> false.
