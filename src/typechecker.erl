-module(typechecker).

%% API used by gradualizer.erl
-export([type_check_forms/2]).

%% Functions used in unit tests.
-export([type_check_expr/2,
         type_check_expr_in/3,
         create_env/2,
         subtype/3,
         normalize/2,
         glb/3,
         type/1, type/2,
         type_diff/3,
         refinable/2,
         compatible/3,
         collect_specs_types_opaques_and_functions/1,
         number_of_exported_functions/1,
         bounded_type_list_to_type/2,
         unfold_bounded_type/2]).

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
                        Index = ?assert_type(size(TypeError), pos_integer()),
                        Index > 0 orelse erlang:error(impossible),
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
              typed_record_field/0]).

-type expr() :: gradualizer_type:abstract_expr().
-type pattern() :: gradualizer_type:abstract_pattern().
-type type() :: gradualizer_type:abstract_type().

-type form() :: erl_parse:abstract_form().
-type forms() :: gradualizer_file_utils:abstract_forms().

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

-type record_field() :: {record_field, erl_anno:anno(),
                         Name :: {atom, erl_anno:anno(), atom()},
                         DefaultValue :: gradualizer_type:abstract_expr()}.
-type typed_record_field() :: {typed_record_field, record_field(), type()}.

%% The environment passed around during typechecking.
%% TODO: See https://github.com/josefs/Gradualizer/issues/364 for details.
%%       Making the type def and record def have the same number of fields fixes a broken Gradualizer
%%       diagnostic, which seems to assume the record only has the
%%       fields annotated in the type, not all the fields from the definition.
-include("typechecker.hrl").
-type env() :: #env{}.

-include("gradualizer.hrl").

-type compatible() :: {true, constraints:constraints()} | false.

-type anno() :: erl_anno:anno().
-type binary_op() :: gradualizer_type:binary_op().
-type bounded_function() :: gradualizer_type:af_constrained_function_type().
-type unary_op() :: gradualizer_type:unary_op().

%% TODO: Some of these don't seem to be thrown at all, e.g. expected_fun_type
-type type_error() :: arith_error | badkey | call_arity | call_intersect | check_clauses | cons_pat
                    | cyclic_type_vars | expected_fun_type | int_error | list | mismatch
                    | no_type_match_intersection | non_number_argument_to_minus
                    | non_number_argument_to_plus | op_type_too_precise | operator_pattern | pattern
                    | receive_after | record_pattern | rel_error | relop | unary_error
                    | unreachable_clause.
-type undef() :: record | user_type | remote_type | record_field.

-type error() :: {type_error, type_error()}
               | {type_error, type_error(), anno()}
               | {type_error, expr(), type() | [type()], type()}
               | {type_error, type_error(), anno(), type()}
               | {type_error, cyclic_type_vars, anno(), bounded_function(), list()}
               | {type_error, type_error(), anno(), atom() | pattern(), type()}
               | {type_error, type_error(), unary_op(), anno(), type()}
               | {type_error, type_error(), binary_op(), anno(), type(), type()}
               | {type_error, call_arity, anno(), atom(), arity(), arity()}
               | {undef, undef(), anno(), {atom(), atom() | arity()} | mfa()}
               | {undef, undef(), expr()}
               | {not_exported, remote_type, anno(), {module(), atom(), arity()}}
               | {bad_type_annotation, gradualizer_type:af_string()}
               | {illegal_map_type, type()}
               | {argument_length_mismatch, anno(), arity(), arity()}
               | {nonexhaustive, anno(), expr()}
               | {illegal_pattern, pattern()}
               | {internal_error, missing_type_spec, atom(), arity()}
               | {call_undef, anno(), module(), atom(), arity()}.
%% `typechecker' returns these errors as results of its analysis.

%% Two types are compatible if one is a subtype of the other, or both.
-spec compatible(type(), type(), env()) -> compatible().
compatible(Ty1, Ty2, Env) ->
    case {subtype(Ty1, Ty2, Env), subtype(Ty2, Ty1, Env)} of
        {{true, C1}, {true, C2}} ->
            {true, constraints:combine(C1,C2)};
        {false, T={true, _C2}} ->
            T;
        {T={true, _C1}, false} ->
            T;
        {false, false} ->
            false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subtyping compatibility
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The first argument is a "compatible subtype" of the second.

-spec subtype(type(), type(), env()) -> compatible().
subtype(Ty1, Ty2, Env) ->
    try compat(Ty1, Ty2, maps:new(), Env) of
        {_Memoization, Constraints} ->
            {true, Constraints}
    catch
        nomatch ->
            false
    end.

%% Check if at least on of the types in a list is a subtype of a type.
%% Used when checking intersection types.
-spec any_subtype(_, _, env()) -> compatible().
any_subtype([], _Ty, _Env) ->
    false;
any_subtype([Ty1|Tys], Ty, Env) ->
    case subtype(Ty1, Ty, Env) of
        R={true, _} ->
            R;
        false ->
            any_subtype(Tys, Ty, Env)
    end.

-type acc(Seen) :: {Seen, constraints:constraints()}.

-type compat_acc() :: acc(map()).

% This function throws an exception in case of a type error

%% The functions compat and compat_ty are mutually recursive.
%% The main entry point is compat and all recursive calls should go via compat.
%% The function compat_ty is just a convenience function to be able to
%% pattern match on types in a nice way.
-spec compat(type(), type(), map(), env()) -> compat_acc().
compat(T1, T2, Seen, Env) ->
    ?assert_normalized_anno(T1),
    ?assert_normalized_anno(T2),
    Ty1 = normalize(T1, Env),
    Ty2 = normalize(T2, Env),
    case compat_seen({T1, T2}, Seen) of
        true ->
            ret(Seen);
        false ->
            compat_ty(Ty1, Ty2, maps:put({T1, T2}, true, Seen), Env)
    end.

compat_seen({T1, T2}, Seen) ->
    maps:get({T1, T2}, Seen, false).

-spec compat_ty(type(), type(), map(), env()) -> compat_acc().
%% any() and term() are used as the unknown type in the gradual type system
compat_ty({type, _, any, []}, _, Seen, _Env) ->
    ret(Seen);
compat_ty(_, {type, _, any ,[]}, Seen, _Env) ->
    ret(Seen);
% gradualizer:top() is the top of the subtyping hierarchy
compat_ty(_, ?top(), Seen, _Env) ->
    ret(Seen);

%% None is the bottom of the subtyping relation
compat_ty({type, _, none, []}, _, Seen, _Env) ->
    ret(Seen);
%% Every type is subtype of itself
compat_ty(T, T, Seen, _Env) ->
    ret(Seen);

%% Variables
compat_ty({var, _, Var}, Ty, Seen, _Env) ->
    {Seen, constraints:upper(Var, Ty)};
compat_ty(Ty, {var, _, Var}, Seen, _Env) ->
    {Seen, constraints:lower(Var, Ty)};

% TODO: There are several kinds of fun types.
% Add support for them all eventually
compat_ty({type, _, 'fun', [_, Res1]},
          {type, _, 'fun', [{type, _, any}, Res2]},
          Seen, Env) ->
    %% We can assert the below,
    %% as we know Res2 is not {type, _, any}, which is explicitely matched on above.
    Res2 = ?assert_type(Res2, type()),
    compat(Res1, Res2, Seen, Env);
compat_ty({type, _, 'fun', [{type, _, product, Args1}, Res1]},
          {type, _, 'fun', [{type, _, product, Args2}, Res2]},
          Seen, Env) ->
    {Ap, Cs} = compat_tys(Args2, Args1, Seen, Env),
    {Aps, Css} = compat(Res1, Res2, Ap, Env),
    {Aps, constraints:combine(Cs, Css)};

%% Unions
compat_ty({type, _, union, Tys1}, {type, _, union, Tys2}, Seen, Env) ->
    lists:foldl(fun (Ty1, {Seen1, C1}) ->
                    {Seen2, C2} = any_type(Ty1, Tys2, Seen1, Env),
                    {Seen2, constraints:combine(C1, C2)}
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
compat_ty({type, Anno1, record, [{atom, _, Name}|Fields1]},
          {type, Anno2, record, [{atom, _, Name}|Fields2]}, Seen, Env) ->
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
compat_ty({type, _, tuple, any}, {type, _, tuple, _Args}, Seen, _Env) ->
    ret(Seen);
compat_ty({type, _, tuple, _Args}, {type, _, tuple, any}, Seen, _Env) ->
    ret(Seen);
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
                                                         (_Assoc1, {Seen1, Cs1}) -> {Seen1, Cs1};
                                                         (Assoc1, nomatch) ->
                                                             try
                                                                 compat(Assoc1, Assoc2, Seen2, Env)
                                                             catch
                                                                 nomatch -> nomatch
                                                             end
                                                     end, nomatch, MandatoryAssocs1)
                                    of
                                        nomatch -> throw(nomatch);
                                        {Seen1, Cs1} -> {Seen1, constraints:combine(Cs1, Cs2)}
                                    end
                            end, ret(Seen), MandatoryAssocs2),
    %% 1. For all associations K1 <Assoc1> V1 in M1,
    %% there exists an association K2 <Assoc2> V2 in M2...
    lists:foldl(fun (Assoc1, {As, Cs1}) ->
                        {Ax, Cs2} = any_type(Assoc1, Assocs2, As, Env),
                        {Ax, constraints:combine(Cs1, Cs2)}
                end, {Seen3, Cs3}, Assocs1);
compat_ty({type, _, AssocTag1, [Key1, Val1]},
          {type, _, AssocTag2, [Key2, Val2]}, Seen, Env)
        when AssocTag1 == map_field_assoc, AssocTag2 == map_field_assoc;
             AssocTag1 == map_field_exact, AssocTag2 == map_field_exact;
             AssocTag1 == map_field_exact, AssocTag2 == map_field_assoc ->
    %% For M1 <: M2, mandatory fields in M2 must be mandatory fields in M1
    {Seen1, Cs1} = compat(Key1, Key2, Seen, Env),
    {Seen2, Cs2} = compat(Val1, Val2, Seen1, Env),
    {Seen2, constraints:combine(Cs1, Cs2)};

%% Opaque user types
compat_ty({user_type, Anno, Name, Args}, {user_type, Anno, Name, Args}, Seen, _Env) ->
    ret(Seen);
compat_ty({user_type, Anno, Name, Args1}, {user_type, Anno, Name, Args2}, Seen, Env)
  when length(Args1) == length(Args2) ->
    lists:foldl(fun ({Arg1, Arg2}, {Seen1, Cs1}) ->
                        {Seen2, Cs2} = compat(Arg1, Arg2, Seen1, Env),
                        {Seen2, constraints:combine(Cs1, Cs2)}
                end, ret(Seen), lists:zip(Args1, Args2));

compat_ty(_Ty1, _Ty2, _, _) ->
    throw(nomatch).

-spec compat_tys([type()], [type()], map(), env()) -> compat_acc().
compat_tys([], [], Seen, _Env) ->
    ret(Seen);
compat_tys([Ty1|Tys1], [Ty2|Tys2], Seen, Env) ->
    {Seen1, Cs} = compat(Ty1 ,Ty2, Seen, Env),
    {Seen2, Css} = compat_tys(Tys1, Tys2, Seen1, Env),
    {Seen2, constraints:combine(Cs, Css)};
compat_tys(_Tys1, _Tys2, _, _) ->
    throw(nomatch).


-spec compat_record_tys([type()], [type()], map(), env()) -> compat_acc().
compat_record_tys([], [], Seen, _Env) ->
    ret(Seen);
compat_record_tys([?type_field_type(Name, Field1)|Fields1], [?type_field_type(Name, Field2)|Fields2], Seen, Env) ->
    {Seen1, Cs1} = compat(Field1, Field2, Seen, Env),
    {Seen2, Cs2} = compat_record_tys(Fields1, Fields2, Seen1, Env),
    {Seen2, constraints:combine(Cs1, Cs2)};
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
    {Seen2, constraints:combine(Cs1, Cs2)};
compat_record_fields(_, _, _, _) ->
    %% Mismatching number of fields
    throw(nomatch).

%% Returns a successful matching of two types. Convenience function for when
%% there were no type variables involved.
-spec ret(Seen) -> acc(Seen) when
      Seen :: map() | type().
ret(Seen) ->
    {Seen, constraints:empty()}.

-spec any_type(type(), [type()], map(), env()) -> compat_acc().
any_type(_Ty, [], _Seen, _Env) ->
    throw(nomatch);
any_type(Ty, [Ty1|Tys], Seen, Env) ->
    try
        compat(Ty, Ty1, Seen, Env)
    catch
        nomatch ->
            any_type(Ty, Tys, Seen, Env)
    end.

%% @doc All types in `Tys' must be compatible with `Ty'.
%% Returns all the gather memoizations and constraints.
%% Does not return (throws `nomatch') if any of the types is not compatible.
-spec all_type([type()], type(), map(), env()) -> compat_acc().
all_type(Tys, Ty, Seen, Env) ->
    all_type(Tys, Ty, Seen, [], Env).

-spec all_type([type()], type(), map(), [constraints:constraints()], env()) -> compat_acc().
all_type([], _Ty, Seen, Css, _Env) ->
    {Seen, constraints:combine(Css)};
all_type([Ty1|Tys], Ty, AIn, Css, Env) ->
    %% TODO: call compat/4 instead of compat_ty/4 here?
    {AOut, Cs} = compat_ty(Ty1, Ty, AIn, Env),
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
-spec get_record_fields(RecName :: atom(),
                        Anno    :: erl_anno:anno(),
                        Env     :: env()) ->
                               [{typed_record_field, _, _}].
get_record_fields(RecName, _Anno, #env{tenv = #{records := REnv}}) ->
    maps:get(RecName, REnv). % It must exist. Otherwise it's a compile error.

%% Greatest lower bound
%% --------------------
%%
%% * Computes the maximal (in the subtyping hierarchy) type that is a subtype
%%   of two given types.

-type glb_acc() :: acc(type()).

-spec glb(type(), type(), env()) -> glb_acc().
glb(T1, T2, Env) ->
    glb(T1, T2, #{}, Env).

-spec glb([type()], env()) -> glb_acc().
glb(Ts, Env) ->
    lists:foldl(fun (T, {TyAcc, Cs1}) ->
                        {Ty, Cs2} = glb(T, TyAcc, Env),
                        {Ty, constraints:combine(Cs1, Cs2)}
                end,
                {top(), constraints:empty()},
                Ts).

-spec glb(type(), type(), map(), env()) -> glb_acc().
glb(T1, T2, A, Env) ->
    case stop_glb_recursion(T1, T2, A) of
        %% If we hit a recursive case we approximate with none(). Conceivably
        %% you could do some fixed point iteration here, but let's wait for an
        %% actual use case.
        true -> {type(none), constraints:empty()};
        false ->
            Module = maps:get(module, Env#env.tenv),
            case gradualizer_cache:get_glb(Module, T1, T2) of
                false ->
                    Ty1 = normalize(T1, Env),
                    Ty2 = normalize(T2, Env),
                    {Ty, Cs} = glb_ty(Ty1, Ty2, A#{ {T1, T2} => 0 }, Env),
                    NormTy = normalize(Ty, Env),
                    gradualizer_cache:store_glb(Module, T1, T2, {NormTy, Cs}),
                    {NormTy, Cs};
                TyCs ->
                    %% these two types have already been seen and calculated
                    TyCs
            end
    end.

%% A standalone function is easier to debug / trace.
stop_glb_recursion(T1, T2, A) ->
    maps:is_key({T1, T2}, A).

-spec glb_ty(type(), type(), map(), env()) -> glb_acc().
%% none() is the bottom of the hierarchy
glb_ty({type, _, none, []} = Ty1, _Ty2, _A, _Env) ->
    ret(Ty1);
glb_ty(_Ty1, {type, _, none, []} = Ty2, _A, _Env) ->
    ret(Ty2);

%% We don't know anything if either type is any()
glb_ty({type, _, any, []} = Ty1, _Ty2, _A, _Env) ->
    ret(Ty1);
glb_ty(_Ty1, {type, _, any, []} = Ty2, _A, _Env) ->
    ret(Ty2);

%% gradualizer:top() is the top of the hierarchy
glb_ty(?top(), Ty2, _A, _Env) ->
    ret(Ty2);
glb_ty(Ty1, ?top(), _A, _Env) ->
    ret(Ty1);

%% glb is idempotent
glb_ty(Ty, Ty, _A, _Env) ->
    ret(Ty);

%% Type variables. TODO: can we get here with constrained type variables?
glb_ty(Var = {var, _, _}, Ty2, _A, _Env) ->
    V = new_type_var(),
    {{var, erl_anno:new(0), V},
     constraints:add_var(V,
                         constraints:combine(constraints:upper(V, Var),
                                             constraints:upper(V, Ty2)))};
glb_ty(Ty1, Var = {var, _, _}, _A, _Env) ->
    V = new_type_var(),
    {{var, erl_anno:new(0), V},
     constraints:add_var(V,
                         constraints:combine(constraints:upper(V, Var),
                                             constraints:upper(V, Ty1)))};

%% Union types: glb distributes over unions
glb_ty({type, Ann, union, Ty1s}, Ty2, A, Env) ->
    {Tys, Css} = lists:unzip([ glb(Ty1, Ty2, A, Env) || Ty1 <- Ty1s ]),
    {{type, Ann, union, Tys}, constraints:combine(Css)};
glb_ty(Ty1, {type, Ann, union, Ty2s}, A, Env) ->
    {Tys, Css} = lists:unzip([glb(Ty1, Ty2, A, Env) || Ty2 <- Ty2s ]),
    {{type, Ann, union, Tys}, constraints:combine(Css)};

%% Atom types
glb_ty(Ty1 = {atom, _, _}, {type, _, atom, []}, _A, _Env) ->
    ret(Ty1);
glb_ty({type, _, atom, []}, Ty2 = {atom, _, _}, _A, _Env) ->
    ret(Ty2);

%% Number types
glb_ty(Ty1, Ty2, _A, _Env) when ?is_int_type(Ty1), ?is_int_type(Ty2) ->
    Glb = gradualizer_int:int_type_glb(Ty1, Ty2),
    ret(Glb);

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
    {Elem, Cs1} = glb(Elem1, Elem2, A, Env),
    {Term, Cs2} = glb(Term1, Term2, A, Env),
    {from_list_view({Empty, Elem, Term}), constraints:combine(Cs1, Cs2)};

%% Tuple types
glb_ty(Ty1 = {type, _, tuple, Tys1}, Ty2 = {type, _, tuple, Tys2}, A, Env) ->
    case {Tys1, Tys2} of
        {any, _} -> ret(Ty2);
        {_, any} -> ret(Ty1);
        _ when length(Tys1) /= length(Tys2) -> ret(type(none));
        _ ->
            {Tys, Css} = lists:unzip(lists:zipwith(fun(T1, T2) ->
                                                           glb(T1, T2, A, Env)
                                                   end,
                                                   Tys1, Tys2)),
            TupleType = case lists:any(fun(?type(none)) -> true; (_) -> false end, Tys) of
                            true ->
                                type(none);
                            false ->
                                type(tuple, Tys)
                        end,
            {TupleType, constraints:combine(Css)}
    end;

%% Record types. Either exactly the same record (handled above) or tuple().
glb_ty(Ty1 = {type, _, record, _}, {type, _, tuple, any}, _A, _Env) ->
    ret(Ty1);
glb_ty({type, _, tuple, any}, Ty2 = {type, _, record, _}, _A, _Env) ->
    ret(Ty2);
glb_ty({type, _, record, _}, {type, _, record, _}, _A, _Env) ->
    ret(type(none));

%% Map types. These are a bit tricky.
%% For now going with a very crude approximation.
glb_ty(Ty1 = {type, _, map, Assocs1}, Ty2 = {type, _, map, Assocs2}, A, Env) ->
    case {Assocs1, Assocs2} of
        %% TODO: add a test case
        {[?any_assoc], _} -> ret(Ty2);
        {_, [?any_assoc]} -> ret(Ty1);
        _ ->
            %% TODO: Too simplistic!
            %% We're not capable of handling overlapping keys without intersection
            %% and negation types!
            case {has_overlapping_keys(Ty1, Env), has_overlapping_keys(Ty2, Env)} of
                {false, false} ->
                    {NewAssocs0, Css} = lists:unzip([ glb(As1, As2, A, Env) || As1 <- Assocs1,
                                                                                As2 <- Assocs2 ]),
                    NewAssocs = lists:filter(fun(?type(none)) -> false; (_) -> true end, NewAssocs0),
                    case NewAssocs of
                        [] ->
                            ret(type(none));
                        [_|_] ->
                            {type(map, NewAssocs), constraints:combine(Css)}
                    end;
                _ ->
                    ret(type(none))
            end
    end;
glb_ty(?type(AssocTag1, [Key1, Val1]), ?type(AssocTag2, [Key2, Val2]), A, Env)
  when AssocTag1 == map_field_assoc, AssocTag2 == map_field_assoc;
       AssocTag1 == map_field_exact, AssocTag2 == map_field_exact;
       AssocTag1 == map_field_exact, AssocTag2 == map_field_assoc;
       AssocTag1 == map_field_assoc, AssocTag2 == map_field_exact ->

    AssocTag = case {AssocTag1, AssocTag2} of
                   {map_field_exact, map_field_exact} -> map_field_exact;
                   {map_field_exact, map_field_assoc} -> map_field_exact;
                   {map_field_assoc, map_field_exact} -> map_field_exact;
                   {map_field_assoc, map_field_assoc} -> map_field_assoc
               end,

    {Key, Cs1} = case {Key1, AssocTag, Key2} of
                     {?type(any), map_field_assoc, ?type(any)} -> ret(type(any));
                     {_, map_field_exact, ?type(any)} -> ret(Key1);
                     {?type(any), map_field_exact, _} -> ret(Key2);
                     {_, _, _} -> glb(Key1, Key2, A, Env)
                     %{_, _, _} -> ret(type(none))
                 end,

    {Val, Cs2} = case {Val1, AssocTag, Val2} of
                     {?type(any), map_field_assoc, ?type(any)} -> ret(type(any));
                     {_, map_field_exact, ?type(any)} -> ret(Val1);
                     {?type(any), map_field_exact, _} -> ret(Val2);
                     {_, _, _} -> glb(Val1, Val2, A, Env)
                     %{_, _, _} -> ret(type(none))
                 end,

    case lists:any(fun(?type(none)) -> true; (_) -> false end, [Key, Val]) of
        true ->
            ret(type(none));
        false ->
            {type(AssocTag, [Key, Val]), constraints:combine(Cs1, Cs2)}
    end;

%% Binary types. For now approximate this by returning the smallest type if
%% they are comparable, otherwise none(). See the corresponding case in
%% compat_ty for the subtyping rule.
glb_ty(Ty1 = {type, _, binary, _},
       Ty2 = {type, _, binary, _}, _A, Env) ->
    case subtype(Ty1, Ty2, Env) of
        {true, _} -> ret(Ty1);    %% Will never produce constraints
        false ->
            case subtype(Ty2, Ty1, Env) of
                {true, _} -> ret(Ty2);
                false     -> ret(type(none))
            end
    end;

%% Function types. Would require lub on arguments for proper implementation.
%% For now pick biggest arguments when comparable and none() otherwise.
glb_ty({type, _, 'fun', [{type, _, product, Args1}, Res1]},
       {type, _, 'fun', [{type, _, product, Args2}, Res2]}, A, Env) ->
    NoConstraints = constraints:empty(),
    {Res, Cs} = glb(Res1, Res2, A, Env),
    Subtype =
        fun(Ts1, Ts2) ->
            try compat_tys(Ts1, Ts2, maps:new(), Env) of
                {_, NoConstraints} -> true;
                _ -> false
            catch throw:nomatch -> false end
        end,
    case Subtype(Args1, Args2) of
        true  -> {type('fun', [type(product, Args2), Res]), Cs};
        false ->
            case Subtype(Args2, Args1) of
                true  -> {type('fun', [type(product, Args1), Res]), Cs};
                false -> {type(none), Cs}
            end
    end;
glb_ty({type, _, 'fun', [{type, _, any} = Any, Res1]},
       {type, _, 'fun', [{type, _, any}, Res2]}, A, Env) ->
    {Res, Cs} = glb(Res1, Res2, A, Env),
    {type('fun', [Any, Res]), Cs};

glb_ty({type, _, 'fun', [{type, _, any}, Res1]},
       {type, _, 'fun', [{type, _, product, _} = TArgs2, _]} = T2, A, Env) ->
    glb(type('fun', [TArgs2, Res1]), T2, A, Env);
glb_ty({type, _, 'fun', [{type, _, product, _} = TArgs1, _]} = T1,
       {type, _, 'fun', [{type, _, any}, Res2]}, A, Env) ->
    glb(T1, type('fun', [TArgs1, Res2]), A, Env);

%% normalize only does the top layer
glb_ty({type, _, Name, Args1}, {type, _, Name, Args2}, A, Env)
        when length(Args1) == length(Args2) ->
    {Args, Css} = lists:unzip([ glb(Arg1, Arg2, A, Env) || {Arg1, Arg2} <- lists:zip(Args1, Args2) ]),
    {type(Name, Args), constraints:combine(Css)};

%% Incompatible
glb_ty(_Ty1, _Ty2, _A, _Env) -> {type(none), constraints:empty()}.

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

%% Normalize
%% ---------
%%
%% * Expand user-defined and remote types on head level (except opaque types)
%% * Replace built-in type synonyms
%% * Flatten unions and merge overlapping types (e.g. ranges) in unions
-spec normalize(type(), env()) -> type().
normalize(Ty, Env) ->
    normalize_rec(Ty, Env, #{}).

%% The third argument is a set of user types that we've already unfolded.
%% It's important that we don't keep unfolding such types because it will
%% lead to infinite recursion.
-spec normalize_rec(type(), env(), map()) -> type().
normalize_rec({type, _, union, Tys} = Type, Env, Unfolded) ->
    case maps:get(mta(Type, Env), Unfolded, no_type) of
        {type, NormType} -> NormType;
        no_type ->
            UnionSizeLimit = Env#env.union_size_limit,
            Types = flatten_unions(Tys, Env, maps:put(Type, {type, Type}, Unfolded)),
            case merge_union_types(Types, Env) of
                []  -> type(none);
                [T] -> T;
                %% Performance hack: Unions larger than this value are replaced by any().
                Ts when length(Ts) > UnionSizeLimit -> type(any);
                Ts  -> type(union, Ts)
            end
    end;
normalize_rec({user_type, _, Name, Args} = Type, Env, Unfolded) ->
    case maps:get(mta(Type, Env), Unfolded, no_type) of
        {type, NormType} -> NormType;
        no_type ->
            UnfoldedNew = maps:put(mta(Type, Env), {type, Type}, Unfolded),
            case gradualizer_lib:get_type_definition(Type, Env, []) of
                {ok, T} ->
                    normalize_rec(T, Env, UnfoldedNew);
                opaque ->
                    Type;
                not_found ->
                    P = position_info_from_spec(Env#env.current_spec),
                    throw(undef(user_type, P, {Name, length(Args)}))
            end
    end;
normalize_rec(T = ?top(), _Env, _Unfolded) ->
    %% Don't normalize gradualizer:top().
    T;
normalize_rec({remote_type, _, [{atom, _, M}, {atom, _, N}, Args]}, Env, Unfolded) ->
    %% It's safe as we explicitly match out `Module :: af_atom()' and `TypeName :: af_atom()'.
    Args = ?assert_type(Args, [type()]),
    P = position_info_from_spec(Env#env.current_spec),
    case gradualizer_db:get_exported_type(M, N, Args) of
        {ok, T} ->
            normalize_rec(T, Env, Unfolded);
        opaque ->
            NormalizedArgs = lists:map(fun (Ty) -> normalize_rec(Ty, Env, Unfolded) end, Args),
            typelib:annotate_user_type(M, {user_type, 0, N, NormalizedArgs});
        not_exported ->
            throw(not_exported(remote_type, P, {M, N, length(Args)}));
        not_found ->
            throw(undef(remote_type, P, {M, N, length(Args)}))
    end;
normalize_rec({op, _, _, _Arg} = Op, _Env, _Unfolded) ->
    erl_eval:partial_eval(Op);
normalize_rec({op, _, _, _Arg1, _Arg2} = Op, _Env, _Unfolded) ->
    erl_eval:partial_eval(Op);
normalize_rec({type, Ann, range, [T1, T2]}, Env, Unfolded) ->
    {type, Ann, range, [normalize_rec(T1, Env, Unfolded),
                        normalize_rec(T2, Env, Unfolded)]};
normalize_rec(Type, _Env, _Unfolded) ->
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
-spec flatten_unions([type()], env(), map()) -> [type()].
flatten_unions(Tys, Env, Unfolded) ->
    [ FTy || Ty <- Tys, FTy <- flatten_type(normalize_rec(Ty, Env, Unfolded), Env, Unfolded) ].

flatten_type({type, _, none, []}, _Env, _Unfolded) ->
    [];
flatten_type({type, _, union, Tys} = Type, Env, Unfolded) ->
    case maps:get(mta(Type, Env), Unfolded, no_type) of
        {type, NormType} -> [NormType];
        no_type ->
            UnfoldedNew = maps:put(mta(Type, Env), {type, Type}, Unfolded),
            flatten_unions(Tys, Env, UnfoldedNew)
    end;
flatten_type(Ty, _Env, _Unfolded) ->
    [Ty].

%% Merges overlapping integer types (including ranges and singletons).
%% (TODO) Removes all types that are subtypes of other types in the same union.
%% Returns a list of disjoint types.
-spec merge_union_types([type()], env()) -> [type()].
merge_union_types(Types, _Env) ->
    case lists:any(fun (?top()) -> true; (_) -> false end, Types) of
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

%% Remove all atom listerals if atom() is among the types.
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
negate_num_type(RangeTy, _Env) ->
    %% some kind of range type like `1..3' or `neg_integer()'
    gradualizer_int:negate_int_type(RangeTy).

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
    Term =
        case Args of
            _ when T == nil; T == list; T == nonempty_list ->
                type(nil);
            [_, B] -> B;
            _ -> type(any)  %% Don't think we get here
        end,
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
          {elem_ty,  type(),   constraints:constraints()}    %% There is exactly one element type
        | {elem_tys, [type()], constraints:constraints()}  %% A union can give rise to multiple elem types
        | any                   %% If we don't know the element type
        | {type_error, type()}. %% If the argument is not compatible with lists

expect_list_type({type, _, T, []}, _, _)
  when T == 'list' orelse T == 'any' orelse
       T == 'nonempty_list' orelse T == 'maybe_improper_list' ->
    any;
expect_list_type({type, _, T, [ElemTy]}, _, _)
  when T == 'list' orelse T == 'nonempty_list' ->
    {elem_ty, ElemTy, constraints:empty()};
expect_list_type(?top() = TermTy, _EmptyOrNot, _) ->
    {elem_ty, TermTy, constraints:empty()};
expect_list_type({type, _, maybe_improper_list, [ElemTy, _]}, _, _) ->
    {elem_ty, ElemTy, constraints:empty()};
expect_list_type({type, _, nil, []}, allow_nil_type, _) ->
    any;
expect_list_type({type, _, string, []}, _, _) ->
    {elem_ty, type(char), constraints:empty()};
expect_list_type(Union = {type, _, union, UnionTys}, N, Env) ->
    {Tys, Cs} = expect_list_union(UnionTys, [], constraints:empty(), no_any, N, Env),
    case Tys of
        [] ->
            {type_error, Union};
        [Ty] ->
            {elem_ty, Ty, Cs};
        _ ->
            {elem_tys, Tys, Cs}
    end;
expect_list_type({var, _, Var}, _, _) ->
    TyVar = new_type_var(),
    {elem_ty
    ,{var, erl_anno:new(0), TyVar}
    ,constraints:add_var(TyVar,
      constraints:upper(Var, {type, erl_anno:new(0), list, [{var, erl_anno:new(0), TyVar}]}))
    };
expect_list_type(Ty, _, _) ->
    {type_error, Ty}.

rewrite_list_to_nonempty_list({type, Ann, T, [ElemTy]})
  when T == list orelse T == nonempty_list ->
    {type, Ann, nonempty_list, [ElemTy]};
rewrite_list_to_nonempty_list({type, Ann, T, [ElemTy, SentinelTy]})
  when T == maybe_improper_list orelse T == nonempty_improper_list ->
    {type, Ann, nonempty_improper_list, [ElemTy, SentinelTy]};
rewrite_list_to_nonempty_list({type, _, any, _} = Ty) ->
    Ty;
rewrite_list_to_nonempty_list(?top()) ->
    top();
rewrite_list_to_nonempty_list({var, _, _} = Var) ->
    Var.

-spec expect_list_union([type()], _, constraints:constraints(), _, _, env()) -> any().
expect_list_union([Ty|Tys], AccTy, AccCs, Any, N, Env) ->
    case expect_list_type(normalize(Ty, Env), N, Env) of
        {type_error, _} ->
            expect_list_union(Tys, AccTy, AccCs, Any, N, Env);
        any ->
            expect_list_union(Tys
                             ,[type(any) | AccTy]
                             ,AccCs
                             ,any
                             ,N
                             ,Env);
        {elem_ty, NTy, Cs} ->
            expect_list_union(Tys
                             ,[NTy | AccTy]
                             ,constraints:combine(Cs, AccCs)
                             ,Any
                             ,N
                             ,Env);
        {elem_tys, NTys, Cs} ->
            expect_list_union(Tys
                             ,NTys ++ AccTy
                             ,constraints:combine(Cs, AccCs)
                             ,Any
                             ,N
                             ,Env)
    end;
expect_list_union([], AccTy, AccCs, any, _N, _Env) ->
    {[type(any) | AccTy], AccCs};
expect_list_union([], AccTy, AccCs, _NoAny, _N, _Env) ->
    {AccTy, AccCs}.

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

expect_tuple_type({type, _, any, []}, _N) ->
    any;
expect_tuple_type({type, _, tuple, any}, _N) ->
    any;
expect_tuple_type({type, _, tuple, Tys}, N) when length(Tys) == N ->
    {elem_ty, Tys, constraints:empty()};
expect_tuple_type(?top() = TermTy, N) ->
    {elem_ty, lists:duplicate(N, TermTy), constraints:empty()};
expect_tuple_type(Union = {type, _, union, UnionTys}, N) ->
    {Tyss, Cs} =
        expect_tuple_union(UnionTys, [], constraints:empty(), no_any, N),
    case Tyss of
        [] ->
            {type_error, Union};
        [Tys] ->
            {elem_ty, Tys, Cs};
        _ ->
            {elem_tys, Tyss, Cs}
    end;
expect_tuple_type({var, _, Var}, N) ->
    TyVars = [ new_type_var() || _ <- lists:seq(1,N) ],
    {elem_ty
    ,[ {var, erl_anno:new(0), TyVar} || TyVar <- TyVars ]
    ,lists:foldr(fun constraints:add_var/2
                ,constraints:upper(Var, type(tuple, TyVars))
                ,TyVars
                )
    };
expect_tuple_type(Ty, _N) ->
    {type_error, Ty}.


expect_tuple_union([Ty|Tys], AccTy, AccCs, Any, N) ->
    case expect_tuple_type(Ty, N) of
        {type_error, _} ->
            expect_tuple_union(Tys, AccTy, AccCs, Any, N);
        any ->
            expect_tuple_union(Tys
                             ,AccTy
                             ,AccCs
                             ,any
                             ,N);
        {elem_ty, TTy, Cs} ->
            expect_tuple_union(Tys
                              ,[TTy | AccTy]
                              ,constraints:combine(Cs, AccCs)
                              ,Any
                              ,N);
        {elem_tys, TTys, Cs} ->
            expect_tuple_union(Tys
                              ,TTys ++ AccTy
                              ,constraints:combine(Cs, AccCs)
                              ,Any
                              ,N)
    end;
expect_tuple_union([], AccTy, AccCs, any, N) ->
    {[ lists:duplicate(N, type(any)) | AccTy], AccCs};
expect_tuple_union([], AccTy, AccCs, _NoAny, _N) ->
    {AccTy, AccCs}.


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

-type fun_ty() :: any
		| {fun_ty, [type()], type(), constraints:constraints()}
		| {fun_ty_any_args, type(), constraints:constraints()}
		| {fun_ty_intersection, [fun_ty()], constraints:constraints()}
		| {fun_ty_union, [fun_ty()], constraints:constraints()}
		.

%% Categorizes a function type.
%% Normalizes the type (expand user-def and remote types). Errors for non-fun
%% types are returned with the original non-normalized type.
%% TODO: move tenv to back
-spec expect_fun_type(env(), type() | [type()]) -> fun_ty() | {type_error, type()}.
expect_fun_type(Env, Type) ->
    Normalized = case Type of
                     Types when is_list(Types) ->
                         [ normalize(Ty, Env) || Ty <- Types ];
                     Type ->
                         normalize(Type, Env)
                 end,
    case expect_fun_type1(Env, Normalized) of
        type_error -> {type_error, Type};
        Other -> Other
    end.

%% TODO: move tenv to back
-spec expect_fun_type1(env(), type() | [type()]) -> fun_ty() | type_error.
expect_fun_type1(Env, BTy = {type, _, bounded_fun, [Ft, _Fc]}) ->
    Sub = bounded_type_subst(Env, BTy),
    case expect_fun_type1(Env, Ft) of
        {fun_ty, ArgsTy, ResTy, Cs} ->
            {fun_ty, subst_ty(Sub, ArgsTy), subst_ty(Sub, ResTy), Cs};
        {fun_ty_any_args, ResTy, Cs} ->
            {fun_ty_any_args, subst_ty(Sub, ResTy), Cs};
        {fun_ty_intersection, Tys, Cs} ->
            {fun_ty_intersection, subst_ty(Sub, Tys), Cs};
        {fun_ty_union, Tys, Cs} ->
            {fun_ty_union, subst_ty(Sub, Tys), Cs};
        Err ->
            Err
    end;
expect_fun_type1(_Env, {type, _, 'fun', [{type, _, product, ArgsTy}, ResTy]}) ->
    {fun_ty, ArgsTy, ResTy, constraints:empty()};
expect_fun_type1(_Env, {type, _, 'fun', []}) ->
    any;
expect_fun_type1(_Env, {type, _, 'fun', [{type, _, any}, ResTy]}) ->
    %% We can assert the below,
    %% as we know Res2 is not {type, _, any}, which is explicitely matched on above.
    ResTy = ?assert_type(ResTy, type()),
    {fun_ty_any_args, ResTy, constraints:empty()};
expect_fun_type1(Env, Tys) when is_list(Tys) ->
    %% This is a spec, not really a type().
    case expect_intersection_type(Env, Tys) of
        type_error ->
            type_error;
        [Ty] ->
            Ty;
        Tyss ->
            {fun_ty_intersection, Tyss, constraints:empty()}
    end;
expect_fun_type1(Env, {type, _, union, UnionTys}) ->
    case expect_fun_type_union(Env, UnionTys) of
        [] ->
            type_error;
        [Ty] ->
            Ty;
        Tys ->
            {fun_ty_union, Tys, constraints:empty()}
    end;
expect_fun_type1(_Env, {var, _, Var}) ->
    ResTy = new_type_var(),
    {fun_ty_any_args, {var, erl_anno:new(0), ResTy}
    ,constraints:add_var(Var,
       constraints:upper(ResTy,
         {type, erl_anno:new(0), 'fun', [{type, erl_anno:new(0), any}
                                        ,{var,  erl_anno:new(0), ResTy}]}))};
expect_fun_type1(_Env, {type, _, any, []}) ->
    any;
expect_fun_type1(_Env, ?top()) ->
    {fun_ty_any_args, top(), constraints:empty()};
expect_fun_type1(_Env, _Ty) ->
    type_error.

-spec expect_intersection_type(env(), [tuple()]) -> [fun_ty()] | type_error.
expect_intersection_type(_Env, []) ->
    [];
expect_intersection_type(Env, [FunTy|Tys]) ->
    case expect_fun_type1(Env, FunTy) of
        type_error ->
            type_error;
        Ty ->
            case expect_intersection_type(Env, Tys) of
                type_error ->
                    type_error;
                Tyss ->
                    [Ty|Tyss]
            end
    end.

-spec expect_fun_type_union(env(), [tuple()]) -> [fun_ty()].
expect_fun_type_union(_Env, []) ->
    [];
expect_fun_type_union(Env, [Ty|Tys]) ->
    case expect_fun_type1(Env, Ty) of
        type_error ->
            expect_fun_type_union(Env, Tys);
        TyOut ->
            [TyOut | expect_fun_type_union(Env, Tys)]
    end.

-spec expect_record_type(type(), atom(), env()) -> Any | FieldsTy | FieldsTys | TypeError when
    Any :: any,
    FieldsTy :: {fields_ty, [typed_record_field()], constraints:constraints()},
    FieldsTys :: {fields_tys, [[typed_record_field()]], constraints:constraints()},
    TypeError :: {type_error, atom() | type()}.
expect_record_type({user_type, _, record, []}, _Record, _Env) ->
    any;
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
            {fields_ty, Tys, constraints:empty()};
        _NotFound ->
            {type_error, Record}
    end;
expect_record_type(?top() = _TermTy, _Record, _Env) ->
    any;
expect_record_type(Union = {type, _, union, UnionTys}, Record, Env) ->
    {Tyss, Cs} =
        expect_record_union(UnionTys, [], constraints:empty(), no_any, Record, Env),
    case Tyss of
        Record ->
            %% expect_record_union failed in every cases
            {type_error, Union};
        [] ->
            {type_error, Union};
        [Tys] ->
            {fields_ty, Tys, Cs};
        _ ->
            {fields_tys, Tyss, Cs}
    end;
expect_record_type({var, _, Var}, Record, Env) ->
    #env{tenv = #{records := REnv}} = Env,
    case REnv of
        #{Record := Fields} ->
            Cs = constraints:add_var(Var, constraints:upper(Var, type_record(Record))),
            {fields_ty, Fields, Cs};
        _NotFound ->
            {type_error, Record}
    end;
expect_record_type({type, _, any, []}, _Record, _Env) ->
    any;
expect_record_type(_, Ty, _) ->
    {type_error, Ty}.

expect_record_union([Ty | Tys], AccTy, AccCs, Any, Record, Env) ->
    case expect_record_type(Ty, Record, Env) of
        {type_error, _} ->
            expect_record_union(Tys, AccTy, AccCs, Any, Record, Env);
        any ->
            expect_record_union(Tys, AccTy, AccCs, any, Record, Env);
        {fields_ty, TTy, Cs} ->
            expect_record_union(Tys, [TTy | AccTy], constraints:combine(Cs, AccCs), Any, Record, Env);
        {fields_tys, TTys, Cs} ->
            expect_record_union(Tys, TTys ++ AccTy, constraints:combine(Cs, AccCs), Any, Record, Env)
    end;
expect_record_union([], AccTy, AccCs, any, _Record, _Env) ->
    {[ type(any) | AccTy], AccCs};
expect_record_union([], AccTy, AccCs, _NoAny, _Record, _Env) ->
    {AccTy, AccCs}.

%% @doc Generate a new type variable.
%%
%% To avoid generating atoms at runtime a string is returned.
-spec new_type_var() -> gradualizer_type:gr_type_var().
new_type_var() ->
    I = erlang:unique_integer(),
    "_TyVar" ++ integer_to_list(I).

%% TODO: move tenv to back
-spec bounded_type_list_to_type(env(), [type()]) -> type().
bounded_type_list_to_type(Env, Types) ->
    case unfold_bounded_type_list(Env, Types) of
        [Ty] -> Ty;
        Tys  -> type(union, Tys)
    end.

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
    subst_ty(Sub, Ty);
unfold_bounded_type(_Env, Ty) -> Ty.

%% TODO: move tenv to back
-spec bounded_type_subst(env(), {type, erl_anno:anno(), bounded_fun, [_]}) ->
        #{ atom() => type() }.
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
    %% TODO: Don't drop the constraints.
    {Ty1, _Cs} =
      case Defs of
	  #{X := Tys} ->
	      Tys1 = subst_ty(Acc, Tys),
	      %% Take intersection after substitution to
	      %% get rid of type variables.
	      lists:foldl(fun(S, {T, Css}) ->
				  {Ty, Cs} = glb(S, T, Env),
				  {Ty, constraints:combine(Cs, Css)}
			  end,
			  {top(), constraints:empty()}, Tys1);
          _NoBoundsForX ->
              %% Unconstrained type variables are kept as type variables.
              {{var, erl_anno:new(0), X}, constraints:empty()}
      end,
    solve_bounds(Env, maps:remove(X, Defs), SCCs, Acc#{ X => Ty1 });
solve_bounds(_, _, [{cyclic, Xs} | _], _) ->
    throw({cyclic_dependencies, Xs});
solve_bounds(_, _, [], Acc) -> Acc.

free_vars(Ty) -> free_vars(Ty, #{}).

free_vars({var, _, '_'}, Vars) ->
    Vars;
free_vars({var, _, X}, Vars) ->
    Vars#{ X => true };
free_vars([H | T], Vars) ->
    free_vars(T, free_vars(H, Vars));
free_vars({type, _, _, Args}, Vars) ->
    free_vars(Args, Vars);
free_vars(_, Vars) -> Vars.

subst_ty(Sub, Ty = {var, _, X}) ->
    maps:get(X, Sub, Ty);
subst_ty(Sub, {type, P, Name, Args}) ->
    {type, P, Name, subst_ty(Sub, Args)};
subst_ty(Sub, Tys) when is_list(Tys) ->
    [ subst_ty(Sub, Ty) || Ty <- Tys ];
subst_ty(_, Ty) -> Ty.

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
%% the expression together with their type and constraints.
-spec type_check_expr(env(), expr()) -> {any(), env(), constraints:constraints()}.
type_check_expr(Env, Expr) ->
    Res = {Ty, _VarBinds, _Cs} = do_type_check_expr(Env, Expr),
    ?verbose(Env, "~sPropagated type of ~ts :: ~ts~n",
             [gradualizer_fmt:format_location(Expr, brief), erl_prettypr:format(Expr), typelib:pp_type(Ty)]),
    Res.

%% TODO: move tenv to back
-spec do_type_check_expr(env(), expr()) -> {any(), env(), constraints:constraints()}.
do_type_check_expr(Env, {var, _P, Var}) ->
    case Env#env.venv of
        #{Var := Ty} ->
            {Ty, Env, constraints:empty()}
    end;
do_type_check_expr(Env, {match, _, Pat, Expr}) ->
    {Ty, VarBinds, Cs} = type_check_expr(Env, Expr),
    NormTy = normalize(Ty, Env),
    NewEnv = union_var_binds(VarBinds, Env, Env),
    {[_PatTy], [UBoundNorm], Env2, Cs2} =
            ?throw_orig_type(add_types_pats([Pat], [NormTy], NewEnv, capture_vars), Ty, NormTy),
    UBound = case UBoundNorm of NormTy -> Ty;
                                _Other -> UBoundNorm end,
    {UBound, Env2, constraints:combine(Cs,Cs2)};
do_type_check_expr(Env, {'if', _, Clauses}) ->
    infer_clauses(Env, Clauses);
do_type_check_expr(Env, {'case', _, Expr, Clauses}) ->
    {_ExprTy, Env1, Cs1} = type_check_expr(Env, Expr),
    Env2 = add_var_binds(Env, Env1, Env),
    {Ty, VB, Cs2} = infer_clauses(Env2, Clauses),
    {Ty, union_var_binds(Env1, VB, Env), constraints:combine(Cs1, Cs2)};
do_type_check_expr(Env, {tuple, _, TS}) ->
    {Tys, VarBindsList, Css} = lists:unzip3([ type_check_expr(Env, Expr) || Expr <- TS ]),
    InferredTy =
        case not Env#env.infer andalso
             lists:all(fun({type, _, any, []}) -> true;
                          (_) -> false
                       end, Tys) of
            true ->
                type(any);
            false ->
                %% at least one element in the tuple has a type inferred from a spec
                type(tuple, Tys)
        end,
    { InferredTy, union_var_binds(VarBindsList, Env), constraints:combine(Css) };
do_type_check_expr(Env, {cons, _, Head, Tail}) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Head),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Tail),
    VB = union_var_binds(VB1, VB2, Env),
    Cs = constraints:combine([Cs1, Cs2]),
    case {Ty1, Ty2} of
        {?type(any), ?type(any)} when not Env#env.infer ->
            %% No type information to propagate
            {type(any), VB, Cs};
        {_, ?type(any)} ->
            %% Propagate type information from head
            {type(nonempty_list, [Ty1]), VB, Cs};
        {_, _} ->
            %% Propagate type information from tail, which must be a list type
            {TailElemTys, Cs4} =
                case expect_list_type(Ty2, dont_allow_nil_type, Env) of
                    any ->
                        {[type(any)], Cs};
                    {elem_ty, ElemTy, Cs3} ->
                        {[ElemTy], constraints:combine([Cs, Cs3])};
                    {elem_tys, ElemTys, Cs3} ->
                        {ElemTys, constraints:combine([Cs, Cs3])};
                    {type_error, ?type(nil)} ->
                        {[], Cs};
                    {type_error, BadTy} ->
                        throw(type_error(list, line_no(Tail), BadTy))
                        %% We throw a type error here because Tail is not of type list
                        %% (nor is it of type any()).
                        %% TODO: Improper list?
                end,
            FinalElemTy = normalize(type(union, [Ty1|TailElemTys]), Env),
            {type(nonempty_list, [FinalElemTy]), VB, Cs4}
    end;
do_type_check_expr(Env, {bin, _, BinElements} = BinExpr) ->
    %% <<Expr:Size/TypeSpecifierList, ...>>
    VarBindAndCsList =
        lists:map(fun ({bin_element, _P, Expr, _Size, _Specif} = BinElem) ->
                          %% Treat bin type specifier as type annotation
                          Ty = type_of_bin_element(BinElem, expr),
                          type_check_expr_in(Env, Ty, Expr)
                  end,
                  BinElements),
    {VarBinds, Css} = lists:unzip(VarBindAndCsList),
    RetTy = if
                Env#env.infer ->
                    %% Infer the size parameters of the bitstring
                    gradualizer_bin:compute_type(BinExpr);
                not Env#env.infer ->
                    type(any)
            end,
    {RetTy,
     union_var_binds(VarBinds, Env),
     constraints:combine(Css)};
do_type_check_expr(Env, {call, _, {atom, _, TypeOp}, [Expr, {string, _, TypeStr} = TypeLit]})
  when TypeOp == '::'; TypeOp == ':::' ->
    %% Magic functions used as type annotation/assertion.
    try typelib:remove_pos(typelib:parse_type(TypeStr)) of
        Type when TypeOp == '::' ->
            {VarBinds, Cs} = type_check_expr_in(Env, Type, Expr),
            {Type, VarBinds, Cs};
        Type when TypeOp == ':::' ->
            {InferredType, VarBinds, Cs1} = type_check_expr(Env, Expr),
            case compatible(InferredType, Type, Env) of
                {true, Cs2} ->
                   {Type, VarBinds, constraints:combine(Cs1, Cs2)};
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
    {Ty, Env, constraints:empty()};
do_type_check_expr(Env, {call, P, Name, Args}) ->
    Arity = length(Args),
    Arity = ?assert_type(Arity, arity()),
    {FunTy, VarBinds1, Cs1} = type_check_fun(Env, Name, Arity),
    {ResTy, VarBinds2, Cs2} = type_check_call_ty(Env, expect_fun_type(Env, FunTy), Args
                                                ,{Name, P, FunTy}),
    {ResTy, union_var_binds(VarBinds1, VarBinds2, Env),
            constraints:combine(Cs1, Cs2)};

do_type_check_expr(Env, {lc, _, Expr, Qualifiers}) ->
    type_check_comprehension(Env, lc, Expr, Qualifiers);
do_type_check_expr(Env, {bc, _, Expr, Qualifiers}) ->
    type_check_comprehension(Env, bc, Expr, Qualifiers);
do_type_check_expr(Env, {block, _, Block}) ->
    type_check_block(Env, Block);

% Don't return the type of anything other than something
% which ultimately comes from a function type spec.
do_type_check_expr(#env{infer = false} = Env, {string, _, _}) ->
    {type(any), Env, constraints:empty()};
do_type_check_expr(#env{infer = false} = Env, {nil, _}) ->
    {type(any), Env, constraints:empty()};
do_type_check_expr(#env{infer = false} = Env, {atom, _, _Atom}) ->
    {type(any), Env, constraints:empty()};
do_type_check_expr(#env{infer = false} = Env, {integer, _, _N}) ->
    {type(any), Env, constraints:empty()};
do_type_check_expr(#env{infer = false} = Env, {float, _, _F}) ->
    {type(any), Env, constraints:empty()};
do_type_check_expr(#env{infer = false} = Env, {char, _, _C}) ->
    {type(any), Env, constraints:empty()};

%% When infer = true, we do propagate the types of literals,
%% list cons, tuples, etc.
do_type_check_expr(#env{infer = true} = Env, {string, _, Chars}) ->
    ActualyTy = infer_literal_string(Chars, Env),
    {ActualyTy, Env, constraints:empty()};
do_type_check_expr(#env{infer = true} = Env, {nil, _}) ->
    {type(nil), Env, constraints:empty()};
do_type_check_expr(#env{infer = true} = Env, {Tag, _, Value})
  when Tag =:= atom;
       Tag =:= integer;
       Tag =:= char ->
    {{Tag, erl_anno:new(0), Value}, Env, constraints:empty()};
do_type_check_expr(#env{infer = true} = Env, {float, _, _F}) ->
    {type(float), Env, constraints:empty()};

%% Maps
do_type_check_expr(Env, {map, _, Assocs}) ->
    {AssocTys, VB, Cs} = type_check_assocs(Env, Assocs),
    % TODO: When the --infer flag is set we should return the type of the map
    case Env#env.infer of
        true ->
            {type(map, AssocTys), VB, Cs};
        false ->
            {type(any), VB, Cs}
    end;
do_type_check_expr(Env, {map, _, UpdateExpr, Assocs}) ->
    {Ty, VBExpr, Cs1} = type_check_expr(Env, UpdateExpr),
    {AssocTys, VBAssocs, Cs2} = type_check_assocs(Env, Assocs),
    MapTy = update_map_type(Ty, AssocTys),
    % TODO: Check the type of the map.
    {MapTy, union_var_binds(VBExpr, VBAssocs, Env), constraints:combine(Cs1, Cs2)};

%% Records
do_type_check_expr(Env, {record_field, Anno, Expr, Record, FieldWithAnno}) ->
    {Ty, VB1, Cs1} = type_check_expr(Env, Expr),
    case Ty of
        {type, _, record, [{atom, _, Record}]} ->
            % Unrefined
            {VB2, Cs2} = type_check_expr_in(Env, {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]}, Expr),
            Rec = get_record_fields(Record, Anno, Env),
            FieldTy = get_rec_field_type(FieldWithAnno, Rec),
            VB = union_var_binds(VB1, VB2, Env),
            Cs = constraints:combine(Cs1, Cs2),
            {FieldTy, VB, Cs};
        {type, _, record, [{atom, _, Record} | Fields]} ->
            Rec = get_record_fields(Record, Anno, Env),
            FieldTypes1 = record_field_types(Fields),
            FieldTypes2 = record_field_types(Rec),
            FieldTy = get_rec_field_type(FieldWithAnno, FieldTypes1, FieldTypes2),
            {FieldTy, VB1, Cs1};
        _ ->
            {VB2, Cs2} = type_check_expr_in(Env, {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]}, Expr),
            Rec = get_record_fields(Record, Anno, Env),
            FieldTy = get_rec_field_type(FieldWithAnno, Rec),
            VB = union_var_binds(VB1, VB2, Env),
            Cs = constraints:combine(Cs1, Cs2),
            {FieldTy, VB, Cs}
    end;
do_type_check_expr(Env, {record, Anno, Expr, Record, Fields}) ->
    RecTy = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    {VB1, Cs1} = type_check_expr_in(Env, RecTy, Expr),
    Rec = get_record_fields(Record, Anno, Env),
    {VB2, Cs2} = type_check_fields_for_update(Env, Rec, Fields),
    {RecTy, union_var_binds(VB1, VB2, Env), constraints:combine(Cs1, Cs2)};
do_type_check_expr(Env, {record, Anno, Record, Fields}) ->
    RecTy    = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    Rec      = get_record_fields(Record, Anno, Env),
    {VB, Cs} = type_check_fields(Env, Rec, Fields),
    {RecTy, VB, Cs};
do_type_check_expr(Env, {record_index, Anno, Name, FieldWithAnno}) ->
    case Env#env.infer of
        true ->
            RecFields = get_record_fields(Name, Anno, Env),
            Index = get_rec_field_index(FieldWithAnno, RecFields),
            {{integer, erl_anno:new(0), Index}, Env, constraints:empty()};
        false ->
            {type(any), Env, constraints:empty()}
    end;

%% Functions
do_type_check_expr(Env, {'fun', _, {clauses, Clauses}}) ->
    type_check_fun(Env, Clauses);
do_type_check_expr(Env, {'fun', P, {function, Name, Arity}}) ->
    case get_bounded_fun_type_list(Name, Arity, Env, P) of
        AnyType = {type, _, any, []} ->
            {AnyType, Env, constraints:empty()};
        BoundedFunTypeList ->
            Ty = bounded_type_list_to_type(Env, BoundedFunTypeList),
            {Ty, Env, constraints:empty()}
    end;
do_type_check_expr(Env, {'fun', P, {function, M, F, A}}) ->
    case {get_atom(Env, M), get_atom(Env, F), A} of
        {{atom, _, Module}, {atom, _, Function}, {integer, _, Arity}} ->
            case gradualizer_db:get_spec(Module, Function, Arity) of
                {ok, BoundedFunTypeList} ->
                    Ty = bounded_type_list_to_type(Env, BoundedFunTypeList),
                    {Ty, Env, constraints:empty()};
                not_found ->
                    throw(call_undef(P, Module, Function, Arity))
            end;
        _ -> %% Not enough information to check the type of the call.
            {type(any), Env, constraints:empty()}
    end;
do_type_check_expr(Env, {named_fun, _, FunName, Clauses}) ->
    %% Pick a type for the fun itself, to be used when checking references to
    %% itself inside the fun, e.g. recursive calls.
    FunTy = if
                Env#env.infer ->
                    %% Create a fun type of the correct arity
                    %% on the form fun((_,_,_) -> any()).
                    [{clause, _, Params, _Guards, _Block} | _] = Clauses,
                    Arity = ?assert_type(length(Params), arity()),
                    create_fun_type(Arity, type(any));
                not Env#env.infer ->
                    type(any)
            end,
    NewEnv = add_var_binds(Env#env{venv = #{FunName => FunTy}}, Env, Env),
    type_check_fun(NewEnv, Clauses);

do_type_check_expr(Env, {'receive', _, Clauses}) ->
    infer_clauses(Env, Clauses);
do_type_check_expr(Env, {'receive', _, Clauses, _After, Block}) ->
    {TyClauses, VarBinds1, Cs1} = infer_clauses(Env, Clauses),
    {TyBlock,   VarBinds2, Cs2} = type_check_block(Env, Block),
    {normalize({type, erl_anno:new(0), union, [TyClauses, TyBlock]}, Env)
    ,union_var_binds(VarBinds1, VarBinds2, Env)
    ,constraints:combine(Cs1, Cs2)};

%% Operators
do_type_check_expr(Env, {op, _, '!', Proc, Val}) ->
    % Message passing is untyped remotely so the types of Proc and Val are not
    % checked. Val is the return value of the expression.
    {_, VB1, Cs1} = type_check_expr(Env, Proc),
    {TyVal, VB2, Cs2} = type_check_expr(Env, Val),
    {TyVal
    ,union_var_binds(VB1, VB2, Env)
    ,constraints:combine(Cs1, Cs2)};
do_type_check_expr(Env, {op, _, 'not', Arg} = Expr) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    BoolTy = type(boolean),
    case subtype(Ty, BoolTy, Env) of
        {true, Cs2} ->
            NormTy = normalize(Ty, Env),
            {negate_bool_type(NormTy), VB, constraints:combine(Cs1, Cs2)};
        false ->
            throw(type_error(Expr, Ty, BoolTy))
    end;
do_type_check_expr(Env, {op, _, 'bnot', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    IntTy = type(integer),
    case subtype(Ty, IntTy, Env) of
        {true, Cs2} ->
            {type(integer), VB, constraints:combine(Cs1, Cs2)};
        false ->
            throw(type_error(Arg, Ty, IntTy))
    end;
do_type_check_expr(Env, {op, P, '+', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    case subtype(Ty, type(number), Env) of
        {true, Cs2} ->
            {Ty, VB, constraints:combine(Cs1, Cs2)};
        false ->
            throw(type_error(non_number_argument_to_plus, P, Ty))
    end;
do_type_check_expr(Env, {op, P, '-', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    case subtype(Ty, type(number), Env) of
        {true, Cs2} ->
            NormTy = normalize(Ty, Env),
            {negate_num_type(NormTy, Env), VB, constraints:combine(Cs1, Cs2)};
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
    {Ty,  VB,   Cs1} = type_check_block(Env, Block),
    Env2 = add_var_binds(VB, Env, Env),
    {TyC, _VB2, Cs2} = infer_clauses(Env2, CaseCs),
    {TyS, _VB3, Cs3} = infer_clauses(Env2, CatchCs),
    Cs4 = case AfterBlock of
              [] ->
                  constraints:empty();
              _ ->
                  {_TyA, _VB4, Cs5} = type_check_block(Env2, AfterBlock),
                  Cs5
          end,
    {normalize({type, erl_anno:new(0), union, [Ty, TyC, TyS]}, Env)
    ,VB
    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])}.

%% Helper for type_check_expr for funs
-spec type_check_fun(env(), _) -> {type(), env(), constraints:constraints()}.
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
    {RetTy, _VB, _Cs} = infer_clauses(Env, Clauses),
    FunTy = case RetTy of
                {type, _, any, []} when not Env#env.infer ->
                    type(any);
                _SomeTypeToPropagate ->
                    %% Create a fun type with the correct arity on the form
                    %% fun((any(), any(), ...) -> RetTy).
                    [{clause, _, Params, _Guards, _Body} | _] = Clauses,
                    Arity = length(Params),
                    Arity = ?assert_type(Arity, arity()),
                    create_fun_type(Arity, RetTy)
            end,
    %% Variable bindings inside the fun clauses are local inside the fun.
    %% TODO: Solve constraints on the vars bound in each clause of the fun
    %% and propagate the rest of the constraints.
    {FunTy, Env, constraints:empty()}.

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
-spec type_check_fields(env(), _, _, _) -> {env(), constraints:constraints()}.
type_check_fields(Env, TypedRecFields, [{record_field, _, {atom, _, _} = FieldWithAnno, Expr} | Fields]
                 ,UnAssignedFields) ->
    FieldTy = get_rec_field_type(FieldWithAnno, TypedRecFields),
    {VB1, Cs1} = type_check_expr_in(Env, FieldTy, Expr),
    {VB2, Cs2} = type_check_fields(Env, TypedRecFields, Fields, UnAssignedFields),
    {union_var_binds(VB1, VB2, Env), constraints:combine(Cs1,Cs2)};
type_check_fields(Env, TypedRecFields, [{record_field, _, {var, _, '_'}, Expr} | _Fields]
                 ,UnAssignedFields) ->
    {VB1, Cs1} = type_check_fields(Env, TypedRecFields
                                  ,[ {record_field, erl_anno:new(0)
                                     ,{atom, erl_anno:new(0), Field}, Expr}
                                     || Field <- UnAssignedFields]
                                  ,should_not_be_inspected),
    {VB1, Cs1};
type_check_fields(Env, _TypedRecFields, [], should_not_be_inspected) ->
    {Env, constraints:empty()};
type_check_fields(Env, TypedRecFields, [], [UnAssignedField|UnAssignedFields]) ->
    FieldTy = get_rec_field_type({atom, erl_anno:new(0), UnAssignedField}, TypedRecFields),
    FieldDefault = get_rec_field_default({atom, erl_anno:new(0), UnAssignedField}, TypedRecFields),
    {VB1, Cs1} = type_check_expr_in(Env, FieldTy, FieldDefault),
    {VB2, Cs2} = type_check_fields(Env, TypedRecFields, [], UnAssignedFields),
    {union_var_binds(VB1, VB2, Env), constraints:combine(Cs1,Cs2)};
type_check_fields(Env, _TypedRecFields, [], []) ->
    {Env, constraints:empty()}.

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

-spec type_check_logic_op(env(), _, _, _) -> {type(), env(), constraints:constraints()}.
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
    {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
    case subtype(Ty1, type(boolean), Env) of
        false ->
            throw(type_error(Arg1, Ty1, type(boolean)));
        {true, Cs2} ->
            {Ty2, VB2, Cs3} = type_check_expr(UnionVarBindsSecondArg(Env, VB1), Arg2),
            % Allow any() in second argument for shortcut operators
            SndArgTy = if Op == 'andalso'; Op == 'orelse' -> type(any);
                          true                            -> type(bool) end,
            case subtype(Ty2, SndArgTy, Env) of
                false ->
                    throw(type_error(Arg2, Ty2, type(boolean)));
                {true, Cs4} ->
                    Inferred =
                        case Op of
                            'andalso' -> type(union, [Ty1, {atom, erl_anno:new(0), false}]);
                            'orelse'  -> type(union, [Ty1, {atom, erl_anno:new(0), true}]);
                            _         -> type(boolean)
                        end,
                    {normalize(Inferred, Env)
                    ,union_var_binds(VB1, VB2, Env)
                    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])}
            end
    end.

-spec type_check_rel_op(env(), _, _, _, _) -> {type(), env(), constraints:constraints()}.
type_check_rel_op(Env, Op, P, Arg1, Arg2) ->
    case {type_check_expr(Env, Arg1)
         ,type_check_expr(Env, Arg2)} of
        {{Ty1, VB1, Cs1}, {Ty2, VB2, Cs2}} ->
            case compatible(Ty1, Ty2, Env) of
                {true, Cs} ->
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
                    ,union_var_binds(VB1, VB2, Env)
                    ,constraints:combine([Cs,Cs1,Cs2])};
                _ ->
                    throw(type_error(relop, Op, P, Ty1, Ty2))
            end
    end.

-spec type_check_arith_op(env(), _, _, _, _) -> {type(), env(), constraints:constraints()}.
type_check_arith_op(Env, Op, P, Arg1, Arg2) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Arg2),

    case compat_arith_type(Ty1, Ty2, Env) of
        false ->
          throw(type_error(arith_error, Op, P, Ty1, Ty2));
        {Ty, Cs3} ->
            {Ty
            ,union_var_binds(VB1, VB2, Env)
            ,constraints:combine([Cs1, Cs2, Cs3])}
    end.

-spec type_check_int_op(env(), _, _, _, _) -> {type(), env(), constraints:constraints()}.
type_check_int_op(Env, Op, P, Arg1, Arg2) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Arg2),

    case compat_arith_type(Ty1, Ty2, Env) of
        false ->
            throw(type_error(int_error, Op, P, Ty1, Ty2));
        {{type, _, Ty, []}, _} when Ty == float orelse Ty == number ->
            throw(type_error(int_error, Op, P, Ty1, Ty2));
        {Ty, Cs3} ->
            {Ty
            ,union_var_binds(VB1, VB2, Env)
            ,constraints:combine([Cs1, Cs2, Cs3])}
    end.

-spec type_check_list_op(env(), _, _) -> {type(), env(), constraints:constraints()}.
type_check_list_op(Env, Arg1, Arg2) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Arg2),
    ListTy = type(list),
    case {subtype(Ty1, ListTy, Env), subtype(Ty2, ListTy, Env)} of
        {{true, Cs3}, {true, Cs4}} ->
            {normalize(type(union, [Ty1, Ty2]), Env),
             union_var_binds(VB1, VB2, Env),
             constraints:combine([Cs1, Cs2, Cs3, Cs4])};
        {false, _} ->
            throw(type_error(Arg1, Ty1, type(list)));
        {_, false} ->
            throw(type_error(Arg2, Ty2, type(list)));
        _ ->
            %% Prevent "Nonexhaustive patterns" when self-gradualizing.
            erlang:error(unreachable)
    end.

-spec type_check_call_ty(env(), _, _, _) -> {type(), env(), constraints:constraints()}.
type_check_call_ty(Env, {fun_ty, ArgsTy, ResTy, Cs}, Args, E) ->
    case {length(ArgsTy), length(Args)} of
        {L, L} ->
            {VarBindsList, Css} =
                lists:unzip(
                  [ type_check_expr_in(Env, ArgTy, Arg)
                    || {ArgTy, Arg} <- lists:zip(ArgsTy, Args)
                  ]),
            {ResTy
            ,union_var_binds(VarBindsList, Env)
            ,constraints:combine([Cs | Css])};
        {LenTy, LenArgs} ->
            P = element(2, E),
            LenTy = ?assert_type(LenTy, arity()),
            LenArgs = ?assert_type(LenArgs, arity()),
            throw(argument_length_mismatch(P, LenTy, LenArgs))
    end;
type_check_call_ty(Env, {fun_ty_any_args, ResTy, Cs}, Args, _E) ->
    {_Tys, VarBindsList, Css} =
        lists:unzip3(
          [ type_check_expr(Env, Arg)
            || Arg <- Args
          ]),
    {ResTy
    ,union_var_binds(VarBindsList, Env)
    ,constraints:combine([Cs | Css])};
type_check_call_ty(Env, any, Args, _E) ->
    {_Tys, VarBindsList, Css} =
        lists:unzip3(
          [ type_check_expr(Env, Arg)
            || Arg <- Args
          ]),
    {type(any)
    ,union_var_binds(VarBindsList, Env)
    ,constraints:combine(Css)};
type_check_call_ty(Env, {fun_ty_intersection, Tyss, Cs}, Args, E) ->
    {ResTy, VarBinds, CsI} = type_check_call_ty_intersect(Env, Tyss, Args, E),
    {ResTy, VarBinds, constraints:combine(Cs, CsI)};
type_check_call_ty(Env, {fun_ty_union, Tyss, Cs}, Args, E) ->
    {ResTy, VarBinds, CsI} = type_check_call_ty_union(Env, Tyss, Args, E),
    {ResTy, VarBinds, constraints:combine(Cs, CsI)};
type_check_call_ty(_Env, {type_error, _}, _Args, {Name, _P, FunTy}) ->
    throw(type_error(Name, FunTy, type('fun'))).

-spec type_check_call_ty_intersect(env(), _, _, _) -> {type(), env(), constraints:constraints()}.
type_check_call_ty_intersect(_Env, [], _Args, {Name, P, FunTy}) ->
    throw(type_error(call_intersect, P, FunTy, Name));
type_check_call_ty_intersect(Env, [Ty | Tys], Args, E) ->
    try
        type_check_call_ty(Env, Ty, Args, E)
    catch
        Error when element(1,Error) == type_error ->
            type_check_call_ty_intersect(Env, Tys, Args, E)
    end.

-spec type_check_call_ty_union(env(), _, _, _) -> {type(), env(), constraints:constraints()}.
type_check_call_ty_union(Env, Tys, Args, E) ->
    {ResTys, VBs, Css} =
        lists:unzip3([type_check_call_ty(Env, Ty, Args, E)
                      || Ty <- Tys]),
    {normalize(type(union, ResTys), Env),
     union_var_binds(VBs, Env),
     constraints:combine(Css)}.

compat_arith_type(Any = {type, _, any, []}, {type, _, any, []}, _Env) ->
    {Any, constraints:empty()};
compat_arith_type(Any = {type, _, any, []}, Ty, Env) ->
    case subtype(Ty, type(number), Env) of
        false ->
            false;
        _ ->
            {Any, constraints:empty()}
    end;
compat_arith_type(Ty, Any = {type, _, any, []}, Env) ->
    case subtype(Ty, type(number), Env) of
        false ->
            false;
        _ ->
            {Any, constraints:empty()}
    end;
compat_arith_type(Ty1, Ty2, Env) ->
    TInteger = type(integer),
    case {subtype(Ty1, TInteger, Env), subtype(Ty2, TInteger, Env)} of
        {{true, C1}, {true, C2}} ->
            {TInteger, constraints:combine(C1, C2)};
        _ ->
            TFloat = type(float),
            case {subtype(Ty1, TFloat, Env), subtype(Ty2, TFloat, Env)} of
                {{true, C1}, {true, C2}} ->
                    {TFloat, constraints:combine(C1, C2)};
                _ ->
                    TNumber = type(number),
                    case {subtype(Ty1, TNumber, Env), subtype(Ty2, TNumber, Env)} of
                        {{true, C1}, {true, C2}} ->
                            {TNumber, constraints:combine(C1, C2)};
                        _ ->
                            false
                    end
            end
    end.

-spec type_check_comprehension(env(), _, expr(), _) -> {type(), env(), constraints:constraints()}.
type_check_comprehension(Env, lc, Expr, []) ->
    {Ty, _VB, Cs} = type_check_expr(Env, Expr),
    RetTy = case Ty of
                {type, _, any, []} when not Env#env.infer ->
                    %% No type information to propagate. We don't infer a
                    %% list type of the list comprehension when inference
                    %% is disabled.
                    type(any);
                _ ->
                    %% Propagate the type information
                    {type, erl_anno:new(0), list, [Ty]}
            end,
    {RetTy, Env, Cs};
type_check_comprehension(Env, bc, Expr, []) ->
    {Ty, _VB, Cs} = type_check_expr(Env, Expr),
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
    {RetTy, Env, Cs};
type_check_comprehension(Env, Compr, Expr, [{generate, _, Pat, Gen} | Quals]) ->
    {Ty,  _,  Cs1} = type_check_expr(Env, Gen),
    case expect_list_type(Ty, allow_nil_type, Env) of
        {elem_ty, ElemTy, Cs} ->
            {_PatTys, _UBounds, NewEnv, Cs2} =
                add_types_pats([Pat], [ElemTy], Env, capture_vars),
            {TyL, VB, Cs3} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
            {TyL, VB, constraints:combine([Cs, Cs1, Cs2, Cs3])};
        any ->
            NewEnv = add_any_types_pat(Pat, Env),
            {TyL, VB, Cs2} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
            {TyL, VB, constraints:combine(Cs1, Cs2)};
        {elem_tys, _ElemTys, Cs} ->
            %% As a hack, we treat a union type as any, just to
            %% allow the program to type check.
            %% TODO: Rewrite the union outside of the comprehension
            NewEnv = add_any_types_pat(Pat, Env),
            {TyL, VB, Cs2} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
            {TyL, VB, constraints:combine([Cs,Cs1,Cs2])};
        {type_error, BadTy} ->
            throw(type_error(Gen, BadTy, type(list)))
    end;
type_check_comprehension(Env, Compr, Expr, [{b_generate, _P, Pat, Gen} | Quals]) ->
    BitStringTy = type(binary, [{integer, erl_anno:new(0), 0},
                                {integer, erl_anno:new(0), 1}]),
    {VarBinds1, Cs1} =
        type_check_expr_in(Env, BitStringTy, Gen),
    {_PatTys, _UBounds, NewEnv, Cs2} =
        add_types_pats([Pat], [BitStringTy], Env, capture_vars),
    {TyL, VarBinds2, Cs3} =
        type_check_comprehension(NewEnv, Compr, Expr, Quals),
    {TyL
    ,union_var_binds(VarBinds1, VarBinds2, Env)
    ,constraints:combine([Cs1, Cs2, Cs3])};
type_check_comprehension(Env, Compr, Expr, [Guard | Quals]) ->
    %% We don't require guards to return a boolean.
    %% This decision is up for debate.
    {_Ty, VarBinds1, Cs1} = type_check_expr(Env, Guard),
    NewEnv = add_var_binds(Env, VarBinds1, Env),
    {TyL, VarBinds2, Cs2} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
    {TyL, union_var_binds(VarBinds1, VarBinds2, Env), constraints:combine(Cs1, Cs2)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checking the type of an expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec type_check_expr_in(Env, ResTy, Expr) -> R when
      Env :: env(),
      ResTy :: type(),
      Expr :: expr(),
      R :: {env(), constraints:constraints()}.
type_check_expr_in(Env, ResTy, Expr) ->
    ?verbose(Env, "~sChecking that ~ts :: ~ts~n",
            [gradualizer_fmt:format_location(Expr, brief), erl_prettypr:format(Expr), typelib:pp_type(ResTy)]),
    NormResTy = normalize(ResTy, Env),
    R = ?throw_orig_type(do_type_check_expr_in(Env, NormResTy, Expr), ResTy, NormResTy),
    ?assert_type(R, {env(), constraints:constraints()}).

-spec do_type_check_expr_in(Env, ResTy, Expr) -> R when
      Env :: env(),
      ResTy :: type(),
      Expr :: expr(),
      R :: {env(), constraints:constraints()}.
do_type_check_expr_in(Env, {type, _, any, []}, Expr) ->
    {_Ty, VB, Cs} = type_check_expr(Env, Expr),
    {VB, Cs};
do_type_check_expr_in(Env, Ty, {var, _, Name} = Var) ->
    VarTy = maps:get(Name, Env#env.venv),
    case subtype(VarTy, Ty, Env) of
        {true, Cs} ->
            {Env, Cs};
        false ->
            throw(type_error(Var, VarTy, Ty))
    end;
do_type_check_expr_in(Env, Ty, {match, _, Pat, Expr}) ->
    {VarBinds, Cs} = type_check_expr_in(Env, Ty, Expr),
    {_PatTys, _UBounds, NewEnv, Cs2} =
        add_types_pats([Pat], [Ty], Env, capture_vars),
    {union_var_binds(VarBinds, NewEnv, Env), constraints:combine(Cs, Cs2)};
do_type_check_expr_in(Env, Ty, Singleton = {Tag, _, Value}) when Tag =:= integer;
                                                                 Tag =:= char;
                                                                 Tag =:= atom ->
    ActualTy = {Tag, erl_anno:new(0), Value},
    case subtype(ActualTy, Ty, Env) of
        {true, Cs} ->
            {Env, Cs};
        false ->
            throw(type_error(Singleton, ActualTy, Ty))
    end;
do_type_check_expr_in(Env, Ty, {float, _, _} = Float) ->
    ExpectedType = type(float),
    case subtype(ExpectedType, Ty, Env) of
        {true, Cs} ->
            {Env, Cs};
        false ->
            throw(type_error(Float, ExpectedType, Ty))
    end;
do_type_check_expr_in(Env, Ty, Cons = {cons, _, H, T}) ->
    case expect_list_type(Ty, dont_allow_nil_type, Env) of
        {elem_ty, ETy, Cs} ->
            {VB1, Cs1} = type_check_expr_in(Env, ETy, H),
            {VB2, Cs2} = type_check_expr_in(Env, allow_empty_list(Ty),  T),
            {union_var_binds(VB1, VB2, Env), constraints:combine([Cs, Cs1, Cs2])};
        {elem_tys, ETys, Cs} ->
            {VB1, Cs1} = type_check_union_in(Env, ETys, H),
            {VB2, Cs2} = type_check_expr_in (Env, allow_empty_list(Ty), T),
            {union_var_binds(VB1, VB2, Env), constraints:combine([Cs, Cs1, Cs2])};
        any ->
            {_Ty, VB1, Cs1} = type_check_expr   (Env, H),
            {     VB2, Cs2} = type_check_expr_in(Env, allow_empty_list(Ty), T),
            {union_var_binds(VB1, VB2, Env), constraints:combine(Cs1, Cs2)};
        {type_error, _} ->
            throw(type_error(Cons, type(nonempty_list), Ty))
    end;
do_type_check_expr_in(Env, Ty, {nil, _} = Nil) ->
    case subtype(type(nil), Ty, Env) of
        {true, Cs} ->
            {Env, Cs};
        false ->
            throw(type_error(Nil, type(nil), Ty))
    end;
do_type_check_expr_in(Env, Ty, {string, _, Chars} = String) ->
    ActualTy = infer_literal_string(Chars, Env),
    case subtype(ActualTy, Ty, Env) of
      {true, Cs} ->
        {Env, Cs};
      false ->
        throw(type_error(String, ActualTy, Ty))
    end;
do_type_check_expr_in(Env, Ty, {bin, _Anno, _BinElements} = Bin) ->
    BinTy = gradualizer_bin:compute_type(Bin),
    Cs1 = case subtype(BinTy, Ty, Env) of
              {true, Cs0} ->
                  Cs0;
              false ->
                  throw(type_error(Bin, BinTy, Ty))
          end,
    {_Ty, VarBinds, Cs2} = type_check_expr(Env, Bin),
    {VarBinds, constraints:combine(Cs1, Cs2)};
do_type_check_expr_in(Env, ResTy, {tuple, _, TS} = Tup) ->
    case expect_tuple_type(ResTy, length(TS)) of
        {elem_ty, Tys, Cs} ->
            {VBs, Css} = lists:unzip([ type_check_expr_in(Env, Ty, Expr)
                                    || {Ty, Expr} <- lists:zip(Tys, TS) ]),
            {union_var_binds(VBs, Env), constraints:combine([Cs|Css])};
        {elem_tys, Tyss, Cs} ->
            case type_check_tuple_union_in(Env, Tyss, TS) of
                none ->
                    {Ty, _VB, _Cs} = type_check_expr(Env#env{infer = true}, Tup),
                    throw(type_error(Tup, Ty, ResTy));
                {VBs, Css} ->
                    {union_var_binds(VBs, Env), constraints:combine([Cs|Css])}
            end;
        any ->
            {_Tys, VBs, Css} = lists:unzip3([type_check_expr(Env, Expr)
                                           || Expr <- TS ]),
            {union_var_binds(VBs, Env), constraints:combine(Css)};
        {type_error, _} ->
                    {Ty, _VB, _Cs} = type_check_expr(Env#env{infer = true}, Tup),
                    throw(type_error(Tup, Ty, ResTy))
    end;

%% Maps
do_type_check_expr_in(Env, ResTy, {map, _, Assocs} = MapCreation) ->
    {AssocTys, VBs, Cs2} = type_check_assocs(Env, Assocs),
    %% We know that the exact type can be fully determined from `MapCreation' expression.
    %% We start from an empty map (`type(map, [])') and use `AssocTys' to build the complete type.
    %% The inferred type will have only non-optional associations (as the fields are already there
    %% when we build the map).
    MapTy = update_map_type(type(map, []), AssocTys),
    case subtype(MapTy, ResTy, Env) of
        {true, Cs1} ->
            {VBs, constraints:combine(Cs1, Cs2)};
        false ->
            throw(type_error(MapCreation, MapTy, ResTy))
    end;
do_type_check_expr_in(Env, ResTy, {map, _, Expr, Assocs} = MapUpdate) ->
    {Ty, VBExpr, Cs1} = type_check_expr(Env, Expr),
    {AssocTys, VBAssocs, Cs2} = type_check_assocs(Env, Assocs),
    UpdatedTy = update_map_type(Ty, AssocTys),
    case subtype(UpdatedTy, ResTy, Env) of
        {true, Cs3} ->
            {union_var_binds(VBExpr, VBAssocs, Env),
             constraints:combine([Cs1, Cs2, Cs3])};
        false ->
            throw(type_error(MapUpdate, UpdatedTy, ResTy))
    end;

%% Records
do_type_check_expr_in(Env, ResTy, {record, Anno, Name, Fields} = Record) ->
    case expect_record_type(ResTy, Name, Env) of
        {fields_ty, Rec, Cs1} ->
            {VarBinds, Cs2} = type_check_fields(Env, Rec, Fields),
            {VarBinds, constraints:combine(Cs1, Cs2)};
        {fields_tys, Tyss, Cs1} ->
            case type_check_record_union_in(Name, Anno, Tyss, Fields, Env) of
                none ->
                    {Ty, _VB, _Cs} = type_check_expr(Env#env{infer = true}, Record),
                    throw(type_error(Record, Ty, ResTy));
                {VBs, Cs2} ->
                    {union_var_binds([VBs], Env), constraints:combine(Cs1, Cs2)}
            end;
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
        {fields_ty, Rec, Cs1} ->
            {VarBindsList, Css}
                = lists:unzip(
                lists:map(fun ({record_field, _, FieldWithAnno, Expr}) ->
                    FieldTy = get_rec_field_type(FieldWithAnno, Rec),
                    type_check_expr_in(Env, FieldTy, Expr)
                          end
                    ,Fields)
            ),
            RecordTy = type_record(Name, [type_field_type(FieldName, Type) || ?typed_record_field(FieldName, Type) <- Rec]),
            {VarBinds, Cs2} = type_check_expr_in(Env, RecordTy, Exp),
            {union_var_binds([VarBinds|VarBindsList], Env)
                ,constraints:combine([Cs1, Cs2|Css])};
        {fields_tys, Tyss, Cs1} ->
            case type_check_record_union_in(Name, Anno, Tyss, Fields, Env) of
                none ->
                    {Ty, _VB, _Cs} = type_check_expr(Env#env{infer = true}, Record),
                    throw(type_error(Record, Ty, ResTy));
                {VBs, Cs2} ->
                    {union_var_binds([VBs], Env), constraints:combine(Cs1, Cs2)}
            end;
        any ->
            Rec = get_record_fields(Name, Anno, Env),
            type_check_fields(Env, Rec, Fields);
        {type_error, _} ->
            throw(type_error(Record,
                             type(record, [{atom, erl_anno:new(0), Name}]),
                             ResTy))
    end;
do_type_check_expr_in(Env, ResTy, {record_field, _Anno, _Expr, _Name, _FieldWithAnno} = RecordField) ->
    {RefinedFieldTy, VarBinds, Cs1} = type_check_expr(Env, RecordField),
    case subtype(RefinedFieldTy, ResTy, Env) of
        {true, Cs2} ->
            {VarBinds, constraints:combine([Cs1,Cs2])};
        false ->
            throw(type_error(RecordField, RefinedFieldTy, ResTy))
    end;
do_type_check_expr_in(Env, ResTy, {record_index, Anno, Name, FieldWithAnno} = RecIndex) ->
    RecFields = get_record_fields(Name, Anno, Env),
    Index = get_rec_field_index(FieldWithAnno, RecFields),
    IndexTy = {integer, erl_anno:new(0), Index},
    case subtype(IndexTy, ResTy, Env) of
        {true, Cs} ->
            {Env, Cs};
        false ->
            throw(type_error(RecIndex, IndexTy, ResTy))
    end;

do_type_check_expr_in(Env, ResTy, {'case', _, Expr, Clauses}) ->
    {ExprTy, VarBinds, Cs1} = type_check_expr(Env, Expr),
    Env1 = add_var_binds(Env, VarBinds, Env),
    {Env2, Cs2} = check_clauses(Env1, [ExprTy], ResTy, Clauses, capture_vars),
    {Env2, constraints:combine(Cs1, Cs2)};
do_type_check_expr_in(Env, ResTy, {'if', _, Clauses}) ->
    {Env1, Cs} = check_clauses(Env, [], ResTy, Clauses, capture_vars),
    {Env1, Cs};
do_type_check_expr_in(Env, ResTy,
                      {call, _, {atom, _, TypeOp},
                             [Expr, {string, _, TypeStr} = TypeLit]} = TyAnno)
  when TypeOp == '::'; TypeOp == ':::' ->
    try typelib:remove_pos(typelib:parse_type(TypeStr)) of
        Type ->
            case subtype(Type, ResTy, Env) of
                {true, Cs1} when TypeOp == '::' ->
                    {VarBinds, Cs2} = type_check_expr_in(Env, Type, Expr),
                    {VarBinds, constraints:combine(Cs1, Cs2)};
                {true, Cs1} when TypeOp == ':::' ->
                    {InferredType, VarBinds, Cs2} = type_check_expr(Env, Expr),
                    case compatible(InferredType, Type, Env) of
                        {true, Cs3} ->
                            {VarBinds, constraints:combine([Cs1, Cs2, Cs3])};
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
        {true, Cs} ->
            {Env, Cs};
        false ->
            throw(type_error(Call, ResTy, Ty))
    end;
do_type_check_expr_in(Env, ResTy, {call, P, Name, Args} = OrigExpr) ->
    Arity = ?assert_type(length(Args), arity()),
    {FunTy, VarBinds, Cs} = type_check_fun(Env, Name, Arity),
    {VarBinds2, Cs2} = type_check_call(Env, ResTy, OrigExpr, expect_fun_type(Env, FunTy), Args,
                                       {P, Name, FunTy}),
    {union_var_binds(VarBinds, VarBinds2, Env), constraints:combine(Cs, Cs2)};
do_type_check_expr_in(Env, ResTy, {lc, P, Expr, Qualifiers} = OrigExpr) ->
    type_check_comprehension_in(Env, ResTy, OrigExpr, lc, Expr, P, Qualifiers);
do_type_check_expr_in(Env, ResTy, {bc, P, Expr, Qualifiers} = OrigExpr) ->
    type_check_comprehension_in(Env, ResTy, OrigExpr, bc, Expr, P, Qualifiers);

%% Functions
do_type_check_expr_in(Env, Ty, {'fun', _, {clauses, Clauses}} = Fun) ->
    case expect_fun_type(Env, Ty) of
        any ->
            {Env, constraints:empty()};
        {fun_ty, ArgsTy, ResTy, Cs1} ->
            {Env1, Cs2} = check_clauses(Env, ArgsTy, ResTy, Clauses, bind_vars),
            {Env1, constraints:combine(Cs1, Cs2)};
        {fun_ty_any_args, ResTy, Cs1} ->
            {Env1, Cs2} = check_clauses(Env, any, ResTy, Clauses, bind_vars),
            {Env1, constraints:combine(Cs1, Cs2)};
        %% TODO: Can this case actually happen?
        {fun_ty_intersection, Tyss, Cs1} ->
            {Env1, Cs2} = check_clauses_intersect(Env, Tyss, Clauses),
            {Env1, constraints:combine(Cs1, Cs2)};
        {fun_ty_union, Tyss, Cs1} ->
            {Env1, Cs2} = check_clauses_union(Env, Tyss, Clauses),
            {Env1, constraints:combine(Cs1, Cs2)};
        {type_error, _} ->
            throw(type_error(Fun, type('fun'), Ty))
    end;
do_type_check_expr_in(Env, ResTy, Expr = {'fun', P, {function, Name, Arity}}) ->
    case get_bounded_fun_type_list(Name, Arity, Env, P) of
        ?type(any) when not Env#env.infer ->
            {Env, constraints:empty()};
        ?type(any) ->
            FunType = create_fun_type(Arity, type(any)),
            case subtype(FunType, ResTy, Env) of
                {true, Cs} -> {Env, Cs};
                false -> throw(type_error(Expr, FunType, ResTy))
            end;
        BoundedFunTypeList ->
            FunTypeList =
                unfold_bounded_type_list(Env, BoundedFunTypeList),
            case any_subtype(FunTypeList, ResTy, Env) of
                {true, Cs} -> {Env, Cs};
                false -> throw(type_error(Expr, FunTypeList, ResTy))
            end
    end;
do_type_check_expr_in(Env, ResTy, Expr = {'fun', P, {function, M, F, A}}) ->
    case {get_atom(Env, M), get_atom(Env, F), A} of
        {{atom, _, Module}, {atom, _, Function}, {integer, _,Arity}} ->
            case gradualizer_db:get_spec(Module, Function, Arity) of
                {ok, BoundedFunTypeList} ->
                    FunTypeList =
                        unfold_bounded_type_list(Env, BoundedFunTypeList),
                    case any_subtype(FunTypeList, ResTy, Env) of
                        {true, Cs} -> {Env, Cs};
                        false -> throw(type_error(Expr, FunTypeList, ResTy))
                    end;
                not_found ->
                    throw(call_undef(P, Module, Function, Arity))
            end;
        _ -> % We don't have enough information to check the type.
            {Env, constraints:empty()}
    end;
do_type_check_expr_in(Env, Ty, {named_fun, _, FunName, Clauses} = Fun) ->
    Env1 = add_var_binds(Env#env{venv = #{ FunName => Ty }}, Env, Env),
    case expect_fun_type(Env, Ty) of
        any ->
            {Env#env{venv = #{ FunName => Ty }}, constraints:empty()};
        {fun_ty, ArgsTy, ResTy, Cs1} ->
            {Env2, Cs2} = check_clauses(Env1, ArgsTy, ResTy, Clauses, bind_vars),
            {Env2, constraints:combine(Cs1, Cs2)};
        {fun_ty_any_args, ResTy, Cs1} ->
            {Env2, Cs2} = check_clauses(Env1, any, ResTy, Clauses, bind_vars),
            {Env2, constraints:combine(Cs1, Cs2)};
        %% TODO: Can this case actually happen?
        {fun_ty_intersection, Tyss, Cs1} ->
            {Env2, Cs2} = check_clauses_intersect(Env1, Tyss, Clauses),
            {Env2, constraints:combine(Cs1, Cs2)};
        {fun_ty_union, Tyss, Cs1} ->
            {Env2, Cs2} = check_clauses_union(Env, Tyss, Clauses),
            {Env2, constraints:combine(Cs1, Cs2)};
        {type_error, _} ->
            throw(type_error(Fun, type('fun'), Ty))
    end;

do_type_check_expr_in(Env, ResTy, {'receive', _, Clauses}) ->
    {Env1, Cs} = check_clauses(Env, [type(any)], ResTy, Clauses, capture_vars),
    {Env1, Cs};
do_type_check_expr_in(Env, ResTy, {'receive', _, Clauses, After, Block}) ->
    VEnv = Env#env.venv,
    {Env1, Cs1} = check_clauses(Env, [type(any)], ResTy, Clauses, capture_vars),
    {Env2, Cs2} = type_check_expr_in(Env1#env{venv = VEnv}, type(integer), After),
    {Env3, Cs3} = type_check_block_in(Env2#env{venv = VEnv}, ResTy, Block),
    {union_var_binds([Env1, Env2, Env3], Env3), constraints:combine([Cs1, Cs2, Cs3])};
do_type_check_expr_in(Env, ResTy, {op, _, '!', Arg1, Arg2}) ->
    % The first argument should be a pid.
    {_,  VarBinds1, Cs1} = type_check_expr(Env, Arg1),
    {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
    {union_var_binds(VarBinds1, VarBinds2, Env), constraints:combine(Cs1,Cs2)};
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
    Cs =
        case CaseCs of
            [] ->
                %% no `of' part, Block must return ResTy
                {_VB,  Cs1} = type_check_block_in(Env, ResTy, Block),
                Cs1;
            _ ->
                %% there is an `of' part, argument (pattern) of each clause must
                %% accept the return type of Block
                {BlockTy, _VB,  Cs1} = type_check_block(Env, Block),
                %% stangely enough variable bindings are not propagated from Block
                %% to CaseCs ("unsafe" compiler complaint)
                {_Env1, Cs2} = check_clauses(Env, [BlockTy], ResTy, CaseCs, capture_vars),
                constraints:combine(Cs1, Cs2)
        end,
    {_Env2, Cs3} = check_clauses(Env, [type(any)], ResTy, CatchCs, capture_vars),
    Cs5 =
        case AfterBlock of
            [] ->
                constraints:empty();
            _ ->
                %% return value of after block is ignored
                {_AfterTy, _VB4, Cs4} = type_check_block(Env, AfterBlock),
                Cs4
        end,
    %% no variable bindings are propagated from a try expression
    %% as that would be "unsafe"
    {Env, constraints:combine([Cs,Cs3,Cs5])}.

-spec type_check_arith_op_in(env(), type(), _, _, _, _) -> {env(), constraints:constraints()}.
type_check_arith_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    type_check_arith_op_in(Env, number, ResTy, Op, P, Arg1, Arg2).

-spec type_check_int_op_in(env(), type(), _, _, _, _) -> {env(), constraints:constraints()}.
type_check_int_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    type_check_arith_op_in(Env, integer, ResTy, Op, P, Arg1, Arg2).

-spec type_check_arith_op_in(env(), atom(), type(), _, _, _, _) -> {env(), constraints:constraints()}.
type_check_arith_op_in(Env, Kind, ResTy, Op, P, Arg1, Arg2) ->
    {ResTy1, Cs} = glb(type(Kind), ResTy, Env),
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
            {_, VB, Cs1} = type_check_arith_op(Env, Op, P, Arg1, Arg2),
            {VB, constraints:combine(Cs, Cs1)};
        _ ->
            case arith_op_arg_types(Op, ResTy1) of
                {ArgTy1, ArgTy2, Cs1} ->
                    {VarBinds1, Cs2} = type_check_expr_in(Env, ArgTy1, Arg1),
                    {VarBinds2, Cs3} = type_check_expr_in(Env, ArgTy2, Arg2),
                    {union_var_binds(VarBinds1, VarBinds2, Env),
                     constraints:combine([Cs, Cs1, Cs2, Cs3])};
                false ->
                    throw(type_error(op_type_too_precise, Op, P, ResTy1))
            end
    end.

%% What types should be pushed into the arguments if checking an operator
%% application against a given type.

%% any() is always fine
arith_op_arg_types(_Op, Ty = {type, _, any, []}) ->
    {Ty, Ty, constraints:empty()};

%% '/' can't produce an integer type
arith_op_arg_types('/', Ty) when ?is_int_type(Ty) ->
    false;

%% integer() is closed under all operators except '/'
arith_op_arg_types(_, Ty = {type, _, integer, []}) ->
    {Ty, Ty, constraints:empty()};

%% float() is closed under non-integer-exclusive operations and accepts
%% any number() for '/'.
%% Some precision lost: if at least one argument is a float() the other can
%% be integer().
arith_op_arg_types(Op, Ty = {type, _, float, []}) ->
    case Op of
        _ when Op == '+'; Op == '*'; Op == '-' ->
            {Ty, Ty, constraints:empty()};
        '/' -> {type(number), type(number), constraints:empty()}
    end;

%% Singleton types are not closed under any operations
arith_op_arg_types(_, {T, _, _}) when T == integer; T == char ->
    false;

%% pos_integer() is closed under '+',  '*', and 'bor'
arith_op_arg_types(Op, Ty = {type, _, pos_integer, []}) ->
    case lists:member(Op, ['+', '*', 'bor']) of
        true -> {Ty, Ty, constraints:empty()};
        false -> false
    end;

%% Special case for integer recursion: pos_integer() - 1 :: non_neg_integer()
arith_op_arg_types('-', ?type(non_neg_integer)) ->
    {type(pos_integer), {integer, erl_anno:new(0), 1}, constraints:empty()};

%% non_neg_integer() are closed under top except '-' and '/'
arith_op_arg_types(Op, Ty = {type, _, non_neg_integer, []}) ->
    case lists:member(Op, ['+', '*', 'div', 'rem', 'band', 'bor', 'bxor']) of
        %% Shift amounts can be negative
        _ when Op == 'bsl'; Op == 'bsr' -> {Ty, type(integer)
					   ,constraints:empty()};
        true -> {Ty, Ty, constraints:empty()};
        false -> false
    end;

%% neg_integer() is only closed under '+'
arith_op_arg_types(Op, Ty = {type, _, neg_integer, []}) ->
    case Op of
        '+' -> {Ty, Ty, constraints:empty()};
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
        {0, B} when Op == 'rem' ->
            TyR = gradualizer_int:int_range_to_type({0, B + 1}),
            {type(non_neg_integer), TyR, constraints:empty()};
        %% bsr and div make things smaller for any non_neg/pos second argument
        {0, _} when Op == 'bsr' -> {Ty, type(non_neg_integer)
				   ,constraints:empty()};
        {0, _} when Op == 'div' -> {Ty, type(pos_integer)
				   ,constraints:empty()};
        {0, B} ->
            case is_power_of_two(B + 1) andalso
                 lists:member(Op, ['band', 'bor', 'bxor']) of
                true -> {Ty, Ty, constraints:empty()};
                false -> false
            end;
        _ -> false
    end;

%% We get normalised types here, so number() is expanded to integer() | float().
arith_op_arg_types(Op, {type, _, union, Tys}) ->
    ArgTys = [ arith_op_arg_types(Op, Ty) || Ty <- Tys ],
    case [ A || A = {_, _, _} <- ArgTys ] of   %% filter failures
        []      -> false;
        ArgTys1 ->
            {LeftArgs, RightArgs, Css} = lists:unzip3(ArgTys1),
            {type(union, LeftArgs)
	    ,type(union, RightArgs)
	    ,constraints:combine(Css)}
    end;

%% When the expected type is a type variable we have to
%% constrain it appropriately. We generate new type variables for the
%% two arguments.
arith_op_arg_types(_Op, VarTy={var, _, TyVar}) ->
    LTyVar = new_type_var(),
    RTyVar = new_type_var(),
    {type_var(LTyVar), type_var(RTyVar),
     constraints:add_var(LTyVar,
     constraints:add_var(RTyVar,
     constraints:combine(
		       [constraints:upper(TyVar, type(number))
		       ,constraints:upper(LTyVar, VarTy)
		       ,constraints:upper(RTyVar, VarTy)
		       ])))};

%% Cases like Op = '-', Ty = neg_integer()
arith_op_arg_types(_Op, _Ty) ->
    false.

-spec type_check_logic_op_in(env(), type(), expr()) -> {env(), constraints:constraints()}.
type_check_logic_op_in(Env, ResTy, {op, _, Op, Arg1, Arg2} = OrigExpr) when Op == 'andalso'; Op == 'orelse' ->
    Bool   = type(boolean),
    Target = case Op of
                 'andalso' -> {atom, erl_anno:new(0), false};
                 'orelse'  -> {atom, erl_anno:new(0), true}
             end,
    case subtype(Target, ResTy, Env) of
        {true, Cs} ->
            {VarBinds1, Cs1} = type_check_expr_in(Env, Bool, Arg1),
            %% variable bindings are propagated from Arg1 to Arg2
            EnvArg2 = union_var_binds(Env, VarBinds1, Env),
            {VarBinds2, Cs2} = type_check_expr_in(EnvArg2, ResTy, Arg2),
            {union_var_binds(VarBinds1, VarBinds2, Env),
             constraints:combine([Cs, Cs1, Cs2])};
        false ->
            {Arg2Ty, _VB, _Cs} = type_check_expr(Env#env{infer = true}, Arg2),
            Ty = type(union, [Arg2Ty, Target]),
            throw(type_error(OrigExpr, Ty, ResTy))
    end;
type_check_logic_op_in(Env, ResTy, {op, _, _, Arg1, Arg2} = OrigExpr) ->
    Bool = type(boolean),
    case subtype(Bool, ResTy, Env) of
        {true, Cs} ->
          {VarBinds1, Cs1} = type_check_expr_in(Env, Bool, Arg1),
          {VarBinds2, Cs2} = type_check_expr_in(Env, Bool, Arg2),
          {union_var_binds(VarBinds1, VarBinds2, Env)
          ,constraints:combine([Cs, Cs1, Cs2])};
        false ->
          throw(type_error(OrigExpr, Bool, ResTy))
    end.

-spec type_check_rel_op_in(env(), type(), expr()) -> {env(), constraints:constraints()}.
type_check_rel_op_in(Env, ResTy, {op, P, Op, Arg1, Arg2} = OrigExpr) ->
    Ty = type(boolean),
    case subtype(Ty, ResTy, Env) of
        {true, Cs0} ->
          {ResTy1, VarBinds1, Cs1} = type_check_expr(Env, Arg1),
          {ResTy2, VarBinds2, Cs2} = type_check_expr(Env, Arg2),
          case compatible(ResTy1, ResTy2, Env) of
              {true, Cs} ->
                  {union_var_binds(VarBinds1, VarBinds2, Env)
                  ,constraints:combine([Cs0, Cs1, Cs2, Cs])};
              false ->
                  throw(type_error(rel_error, Op, P, ResTy1, ResTy2))
          end;
        false ->
          throw(type_error(OrigExpr, Ty, ResTy))
    end.

-spec type_check_list_op_in(env(), type(), expr()) -> {env(), constraints:constraints()}.
type_check_list_op_in(Env, ResTy, {op, P, Op, Arg1, Arg2} = Expr) ->
    Target =
        %% '--' always produces a proper list, but '++' gives you an improper
        %% list if the second argument is improper.
        case Op of
            '--' -> type(list, [top()]);
            '++' -> type(maybe_improper_list, [top()
					      ,top()])
        end,
    {ResTy1, Cs} = glb(Target, ResTy, Env),
    case ResTy1 of
        {type, _, none, []} ->
            % Tested by test/should_fail/list_op.erl subtract/1
            throw(type_error(Expr, type(list), ResTy));
        {type, _, any, []} ->
            {_, VB, Cs1} = type_check_list_op(Env, Arg1, Arg2),
            {VB, constraints:combine(Cs, Cs1)};
        _ ->
            case list_op_arg_types(Op, ResTy1) of
                {ArgTy1, ArgTy2} ->
                    {VarBinds1, Cs1} = type_check_expr_in(Env, ArgTy1, Arg1),
                    {VarBinds2, Cs2} = type_check_expr_in(Env, ArgTy2, Arg2),
                    {union_var_binds(VarBinds1, VarBinds2, Env),
                     constraints:combine([Cs, Cs1, Cs2])};
                false ->
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

-spec type_check_unary_op_in(env(), type(), _, _, _) -> {env(), constraints:constraints()}.
type_check_unary_op_in(Env, ResTy, Op, P, Arg) ->
    Target =
        case Op of
            'not'  -> type(boolean);
            'bnot' -> type(integer);
            '+'    -> type(number);
            '-'    -> type(number)
        end,
    {ResTy1, Cs} = glb(Target, ResTy, Env),
    case ResTy1 of
        %% TODO: allow if ResTy == none()?
        {type, _, none, []} ->
            throw(type_error(unary_error, Op, P, Target, ResTy));
        _ ->
            ArgTy = unary_op_arg_type(Op, ResTy1),
            {VB, Cs1} = type_check_expr_in(Env, ArgTy, Arg),
            {VB, constraints:combine(Cs, Cs1)}
    end.

%% Which type should we check the argument against if we want the given type
%% out? We already know that Ty is a subtype of the return type of the operator.
unary_op_arg_type('+', Ty) -> Ty;
unary_op_arg_type(Op, {type, _, union, Tys}) ->
    type(union, [ unary_op_arg_type(Op, Ty) || Ty <- Tys ]);
unary_op_arg_type('not', {atom, _, B}) ->
    {atom, erl_anno:new(0), not B}; %% boolean() = false | true
unary_op_arg_type(Op, Ty) when ?is_int_type(Ty), Op == '-' orelse Op == 'bnot' ->
    {Lo, Hi} = gradualizer_int:int_type_to_range(Ty),
    %% TODO: Move this logic to gradualizer_int?
    Neg = fun(pos_inf)             -> neg_inf;
             (neg_inf)             -> pos_inf;
             (N) when Op == '-'    -> -N;
             (N) when Op == 'bnot' -> bnot N end,
    gradualizer_int:int_range_to_type({Neg(Hi), Neg(Lo)});
unary_op_arg_type('-', Ty = {type, _, float, []}) ->
    Ty;
unary_op_arg_type(_Op, {var, _, _}) ->
    %% TODO: this should be more specific once we're able to solve constraints on type vars.
    %%       Otherwise, we'll lose some feedback / precision.
    %%       With constraint solving, if the type variable resolves to pos_integer()
    %%       and _Op == '-' here, then we should have returned neg_integer().
    type(any).

%% Type check list comprehension or a binary comprehension
-spec type_check_comprehension_in(Env        :: env(),
                                  ResTy      :: type(),
                                  OrigExpr   :: gradualizer_type:abstract_expr(),
                                  Compr      :: lc | bc,
                                  Expr       :: gradualizer_type:abstract_expr(),
                                  Position   :: erl_anno:anno(),
                                  Qualifiers :: [ListGen | BinGen | Filter]) ->
        {env(), constraints:constraints()}
       when
        ListGen :: {generate, erl_anno:anno(), gradualizer_type:abstract_expr(), gradualizer_type:abstract_expr()},
        BinGen  :: {b_generate, erl_anno:anno(), gradualizer_type:abstract_expr(), gradualizer_type:abstract_expr()},
        Filter  :: gradualizer_type:abstract_expr().
type_check_comprehension_in(Env, ResTy, OrigExpr, lc, Expr, _P, []) ->
    case expect_list_type(ResTy, allow_nil_type, Env) of
        any ->
            {_Ty, _VB, Cs} = type_check_expr(Env, Expr),
            {Env, Cs};
        {elem_ty, ElemTy, Cs1} ->
            {_VB, Cs2} = type_check_expr_in(Env, ElemTy, Expr),
            {Env, constraints:combine(Cs1, Cs2)};
        {elem_tys, ElemTys, Cs1} ->
            {VB, Cs2} = type_check_union_in(Env, ElemTys, Expr),
            {VB, constraints:combine(Cs1, Cs2)};
        {type_error, _} ->
            throw(type_error(OrigExpr, type(list), ResTy))
    end;
type_check_comprehension_in(Env, ResTy, OrigExpr, bc, Expr, _P, []) ->
    ExprTy = case ResTy of
                 {type, _, binary, [{integer, _, 0}, {integer, _, _N}]} ->
                     %% The result is a multiple of N bits.
                     %% Expr must be a multiple of N bits too.
                     ResTy;
                 {type, _, binary, [{integer, _, M}, {integer, _, _N}]}
                   when M > 0 ->
                     %% The result is a binary with a minimum size of M. This
                     %% requires that the generators are non-empty. We don't
                     %% check that. At least, we can check that Gen is a
                     %% bitstring.
                     {type, erl_anno:new(0), binary,
                            [{integer, erl_anno:new(0), 0},
                             {integer, erl_anno:new(0), 1}]};
                 _ ->
                     Ty = {type, erl_anno:new(0), binary,
                            [{integer, erl_anno:new(0), 0},
                             {integer, erl_anno:new(0), 1}]},
                     throw(type_error(OrigExpr, Ty, ResTy))
             end,
    {_VB, Cs} = type_check_expr_in(Env, ExprTy, Expr),
    {Env, Cs};
type_check_comprehension_in(Env, ResTy, OrigExpr, Compr, Expr, P,
                            [{generate, _, Pat, Gen} | Quals]) ->
    {Ty, _VB1, Cs1} = type_check_expr(Env, Gen),
    case expect_list_type(Ty, allow_nil_type, Env) of
        any ->
            NewEnv = add_any_types_pat(Pat, Env),
            {_VB2, Cs2} = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
            {Env, constraints:combine(Cs1, Cs2)};
        {elem_ty, ElemTy, Cs} ->
            {_PatTys, _UBounds, NewEnv, Cs2} =
                add_types_pats([Pat], [ElemTy], Env, capture_vars),
            {_VB2, Cs3} = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
            {Env, constraints:combine([Cs, Cs1, Cs2, Cs3])};
        {elem_tys, _ElemTys, Cs} ->
            %% TODO: As a hack, we treat a union type as any, just to
            %% allow the program to type check.
            NewEnv = add_any_types_pat(Pat, Env),
            {_VB2, Cs2} = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
            {Env, constraints:combine([Cs, Cs1, Cs2])};
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
    {_VarBindsGen, Cs1} = type_check_expr_in(Env, BitTy, Gen),
    {_PatTys, _UBounds, NewEnv, Cs2} =
        add_types_pats([Pat], [BitTy], Env, capture_vars),
    {VarBinds, Cs3} = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
    {VarBinds, constraints:combine([Cs1, Cs2, Cs3])};
type_check_comprehension_in(Env, ResTy, OrigExpr, Compr, Expr, P, [Pred | Quals]) ->
    %% We choose to check the type of the predicate here. Arguments can be
    %% made either way on whether we should check the type here.
    %% TODO: As a non-boolean predicates don't give runtime errors, we could
    %% possibly add a configuration parameter to toggle this check.
    {VB1, Cs1} = type_check_expr_in(Env, {type, erl_anno:new(0), 'boolean', []}, Pred),
    Env1 = union_var_binds([VB1], Env),
    {VB2, Cs2} = type_check_comprehension_in(Env1, ResTy, OrigExpr, Compr, Expr, P, Quals),
    {union_var_binds([VB2], Env1), constraints:combine(Cs1, Cs2)}.

-spec type_check_assocs(env(), _) -> {[type()], env(), constraints:constraints()}.
type_check_assocs(Env, [{Assoc, _P, Key, Val}| Assocs])
  when Assoc == map_field_assoc orelse Assoc == map_field_exact ->
    {KeyTy, _KeyVB, Cs1} = type_check_expr(Env#env{infer = true}, Key),
    {ValTy, _ValVB, Cs2} = type_check_expr(Env#env{infer = true}, Val),
    {AssocTys, VB, Cs}   = type_check_assocs(Env, Assocs),
    {[type(Assoc, [KeyTy, ValTy]) | AssocTys], VB
    ,constraints:combine([Cs, Cs1, Cs2])};
type_check_assocs(Env, []) ->
    {[], Env, constraints:empty()}.

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

take_assoc(Key, [Assoc = ?type(_, [Key, _VTy])|T], L) ->
    {value, Assoc, lists:reverse(L, T)};
take_assoc(Key, [H|Assocs], L) ->
    take_assoc(Key, Assocs, [H|L]);
take_assoc(_, [], _) ->
    false.

-spec type_check_fun(env(), expr(), arity()) -> {[type()], env(), constraints:constraints()}.
type_check_fun(Env, {atom, P, Name}, Arity) ->
    % Local function call
    Types = get_bounded_fun_type_list(Name, Arity, Env, P),
    {Types, Env, constraints:empty()};
type_check_fun(Env, {remote, P, {atom,_,Module}, {atom,_,Fun}}, Arity) ->
    % Module:function call
    case gradualizer_db:get_spec(Module, Fun, Arity) of
        {ok, Types} -> {Types, Env, constraints:empty()};
        not_found   -> throw(call_undef(P, Module, Fun, Arity))
    end;
type_check_fun(Env, {remote, _, _Expr, _}, Arity)->
    % Call to an unknown module. Revert to dynamic types.
    FunTy = {type, erl_anno:new(0), bounded_fun,
             [{type, erl_anno:new(0), 'fun',
               [{type, erl_anno:new(0), product,
                 lists:duplicate(Arity, type(any))},
                {type,0,any,[]}]},
              []]},
    {[FunTy], Env, constraints:empty()};
type_check_fun(Env, Expr, _Arity) ->
    type_check_expr(Env, Expr).

-spec type_check_call_intersection(env(), type(), _, _, _, _) -> {env(), constraints:constraints()}.
type_check_call_intersection(Env, ResTy, OrigExpr, [Ty], Args, E) ->
    type_check_call(Env, ResTy, OrigExpr, Ty, Args, E);
type_check_call_intersection(Env, ResTy, OrigExpr, Tys, Args, E) ->
    type_check_call_intersection_(Env, ResTy, OrigExpr, Tys, Args, E).

-spec type_check_call_intersection_(env(), type(), _, _, _, _) -> {env(), constraints:constraints()}.
type_check_call_intersection_(_Env, _ResTy, _, [], _Args, {P, Name, Ty}) ->
    throw(type_error(no_type_match_intersection, P, Name, Ty));
type_check_call_intersection_(Env, ResTy, OrigExpr, [Ty | Tys], Args, E) ->
    try
        type_check_call(Env, ResTy, OrigExpr, Ty, Args, E)
    catch
        Error when element(1, Error) == type_error ->
            type_check_call_intersection_(Env, ResTy, OrigExpr, Tys, Args, E)
    end.

-spec type_check_call(env(), type(), _, _, _, _) -> {env(), constraints:constraints()}.
type_check_call(_Env, _ResTy, _, {fun_ty, ArgsTy, _FunResTy, _Cs}, Args, {P, Name, _})
        when length(ArgsTy) /= length(Args) ->
    LenTys = ?assert_type(length(ArgsTy), arity()),
    LenArgs = ?assert_type(length(Args), arity()),
    throw(type_error(call_arity, P, Name, LenTys, LenArgs));
type_check_call(Env, ResTy, OrigExpr, {fun_ty, ArgsTy, FunResTy, Cs}, Args, _) ->
    {VarBindsList, Css} =
        lists:unzip(
          lists:zipwith(fun (ArgTy, Arg) ->
                                type_check_expr_in(Env, ArgTy, Arg)
                        end, ArgsTy, Args)
         ),
    case subtype(FunResTy, ResTy, Env) of
        {true, Cs1} ->
            { union_var_binds(VarBindsList, Env)
            , constraints:combine([Cs, Cs1 | Css]) };
        false ->
            throw(type_error(OrigExpr, FunResTy, ResTy))
    end;
type_check_call(Env, ResTy, OrigExpr, {fun_ty_any_args, FunResTy, Cs}, Args, _)  ->
    {_Tys, VarBindsList, Css} =
        lists:unzip3(
          lists:map(fun (Arg) ->
                            type_check_expr(Env, Arg)
                    end, Args)
         ),
    case subtype(FunResTy, ResTy, Env) of
        {true, Cs1} ->
            { union_var_binds(VarBindsList, Env)
            , constraints:combine([Cs, Cs1 | Css]) };
        false ->
            throw(type_error(OrigExpr, FunResTy, ResTy))
    end;
type_check_call(Env, _ResTy, _, any, Args, _E) ->
    {_Tys, VarBindsList, Css} =
        lists:unzip3(
          lists:map(fun (Arg) ->
                            type_check_expr(Env, Arg)
                    end, Args)
         ),
    {union_var_binds(VarBindsList, Env), constraints:combine(Css)};
type_check_call(Env, ResTy, OrigExpr, {fun_ty_intersection, Tys, Cs1}, Args, E) ->
    {VB, Cs2} = type_check_call_intersection(Env, ResTy, OrigExpr, Tys, Args, E),
    {VB, constraints:combine(Cs1, Cs2)};
type_check_call(Env, ResTy, OrigExpr, {fun_ty_union, Tys, Cs1}, Args, E) ->
    {VB, Cs2} = type_check_call_union(Env, ResTy, OrigExpr, Tys, Args, E),
    {VB, constraints:combine(Cs1, Cs2)};
type_check_call(_Env, _ResTy, _, {type_error, _}, _Args, {_, Name, FunTy}) ->
    throw(type_error(Name, FunTy, type('fun'))).


-spec type_check_call_union(env(), _, _, _, _, _) -> {env(), constraints:constraints()}.
type_check_call_union(Env, _ResTy, _, [], _Args, _E) ->
    {Env, constraints:empty()};
type_check_call_union(Env, ResTy, OrigExpr, [Ty|Tys], Args, E) ->
    {VB1, Cs1} = type_check_call(Env, ResTy, OrigExpr, Ty, Args, E),
    {VB2, Cs2} = type_check_call_union(Env, ResTy, OrigExpr, Tys, Args, E),
    %% TODO: It's not clear to me what should be returned here.
    %% When combining all the varbinds we should really create
    %% a union of all types for a variable.
    {union_var_binds(VB1, VB2, Env), constraints:combine(Cs1, Cs2)}.



-spec type_check_block(env(), [expr()]) -> {type(), env(), constraints:constraints()}.
type_check_block(Env, [Expr]) ->
    type_check_expr(Env, Expr);
type_check_block(Env, [Expr | Exprs]) ->
    {_, VarBinds, Cs1} = type_check_expr(Env, Expr),
    {Ty, VB, Cs2} = type_check_block(add_var_binds(Env, VarBinds, Env), Exprs),
    {Ty, add_var_binds(VB, VarBinds, Env), constraints:combine(Cs1, Cs2)}.

-spec type_check_block_in(env(), type(), [expr()]) -> {env(), constraints:constraints()}.
type_check_block_in(Env, ResTy, [Expr]) ->
    type_check_expr_in(Env, ResTy, Expr);
type_check_block_in(Env, ResTy, [Expr | Exprs]) ->
    {_, VarBinds, Cs1} = type_check_expr(Env, Expr),
    {VB, Cs2} = type_check_block_in(add_var_binds(Env, VarBinds, Env), ResTy, Exprs),
    {add_var_binds(VB, VarBinds, Env), constraints:combine(Cs1, Cs2)}.

-spec type_check_union_in(env(), [type()], expr()) -> {env(), constraints:constraints()}.
type_check_union_in(Env, Tys, Expr) ->
    case type_check_union_in1(Env, Tys, Expr) of
        none        -> throw(type_error(mismatch, type(union, Tys), Expr));
        Ok = {_, _} -> Ok
    end.

-spec type_check_union_in1(env(), [type()], expr()) -> {env(), constraints:constraints()} | none.
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
      R :: {[env()], [constraints:constraints()]} | none.
type_check_tuple_union_in(Env, [Tys|Tyss], Elems) ->
    try
        lists:unzip([type_check_expr_in(Env, Ty, Expr)
                   || {Ty, Expr} <- lists:zip(Tys, Elems)])
    catch
        E when element(1,E) == type_error ->
            type_check_tuple_union_in(Env, Tyss, Elems)
    end;
type_check_tuple_union_in(_Env, [], _Elems) ->
    none.

-spec type_check_record_union_in(Name, Anno, Tyss, Fields, Env) -> R when
      Name :: atom(),
      Anno :: anno(),
      Tyss :: [[typed_record_field()]],
      Fields :: [expr()],
      Env :: env(),
      R :: {env(), constraints:constraints()} | none.
type_check_record_union_in(Name, Anno, [?type(any) | Tyss], Fields, Env) ->
    Rec = get_record_fields(Name, Anno, Env),
    type_check_record_union_in(Name, Anno, [Rec | Tyss], Fields, Env);
type_check_record_union_in(Name, Anno, [Tys|Tyss], Fields, Env) ->
    try
        type_check_fields(Env, Tys, Fields)
    catch
        E when element(1, E) == type_error ->
            type_check_record_union_in(Name, Anno, Tyss, Fields, Env)
    end;
type_check_record_union_in(_Name, _Anno, [], _Fields, _Env) ->
    none.

get_bounded_fun_type_list(Name, Arity, Env, P) ->
    case maps:find({Name, Arity}, Env#env.fenv) of
        %% TODO: https://github.com/josefs/Gradualizer/issues/388
        {ok, Types} when is_list(Types) ->
            [ typelib:remove_pos(Ty) || Ty <- Types ];
        {ok, Type} ->
            typelib:remove_pos(Type);
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


%% Infers (or at least propagates types from) fun/receive/try/case/if clauses.
-spec infer_clauses(env(), [gradualizer_type:abstract_clause()]) ->
        {type(), VarBinds :: env(), constraints:constraints()}.
infer_clauses(Env, Clauses) ->
    {Tys, VarBindsList, Css} =
        lists:unzip3(lists:map(fun (Clause) ->
                                       infer_clause(Env, Clause)
                               end, Clauses)),
    {normalize(type(union, Tys), Env)
    ,union_var_binds(VarBindsList, Env)
    ,constraints:combine(Css)}.

-spec infer_clause(env(), gradualizer_type:abstract_clause()) ->
        {type(), VarBinds :: env(), constraints:constraints()}.
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
    {Ty, VB, Cs} = type_check_block(EnvNew, Block),
    {Ty, union_var_binds(VB, EnvNew, EnvNew), Cs}.


check_clauses_intersect(Env, Ty, Clauses) when not is_list(Ty) ->
    check_clauses_fun(Env, Ty, Clauses);
check_clauses_intersect(Env, [], _Clauses) ->
    {Env, constraints:empty()};
check_clauses_intersect(Env, [Ty|Tys], Clauses) ->
    %% Variable bindings should not leak into subsequent clauses,
    %% that's why we explicitely pass them as appropriate.
    VEnv = Env#env.venv,
    {Env1, Cs1} = check_clauses_fun(Env, Ty, Clauses),
    {Env2, Cs2} = check_clauses_intersect(Env1#env{venv = VEnv}, Tys, Clauses),
    {union_var_binds(Env1, Env2, Env), constraints:combine(Cs1, Cs2)}.

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


-spec check_clauses_fun(Env, FunTy, Clauses) -> R when
      Env :: env(),
      FunTy :: _,
      Clauses :: [gradualizer_type:abstract_clause()],
      R :: {env(), constraints:constraints()}.
check_clauses_fun(Env, {fun_ty, ArgsTy, FunResTy, Cs1}, Clauses) ->
    {Env1, Cs2} = check_clauses(Env, ArgsTy, FunResTy, Clauses, bind_vars),
    {Env1, constraints:combine(Cs1, Cs2)};
check_clauses_fun(Env, {fun_ty_any_args, FunResTy, Cs1}, Clauses) ->
    {Env1, Cs2} = check_clauses(Env, any, FunResTy, Clauses, bind_vars),
    {Env1, constraints:combine(Cs1, Cs2)};
check_clauses_fun(Env, any, Clauses) ->
    check_clauses(Env, any, type(any), Clauses, bind_vars);
check_clauses_fun(Env, {fun_ty_intersection, Tys, Cs1}, Clauses) ->
    {Env1, Cs2} = check_clauses_intersect(Env, Tys, Clauses),
    {Env1, constraints:combine(Cs1, Cs2)};
check_clauses_fun(Env, {fun_ty_union, Tys, Cs1}, Clauses) ->
    {Env1, Cs2} = check_clauses_union(Env, Tys, Clauses),
    {Env1, constraints:combine(Cs1, Cs2)}.

%% Checks a list of clauses (if/case/fun/try/catch/receive).
-spec check_clauses(Env, ArgsTy, ResTy, Clauses, Caps) -> R when
      Env :: env(),
      ArgsTy :: [type()] | any,
      ResTy :: type(),
      Clauses :: [gradualizer_type:abstract_clause()],
      Caps :: capture_vars | bind_vars,
      R :: {env(), constraints:constraints()}.
check_clauses(Env, any, ResTy, [{clause, _, Args, _, _} | _] = Clauses, Caps) ->
    %% 'any' is the ... in the type fun((...) -> ResTy)
    ArgsTy = lists:duplicate(length(Args), type(any)),
    check_clauses(Env, ArgsTy, ResTy, Clauses, Caps);
check_clauses(Env, ArgsTy, ResTy, Clauses, Caps) ->
    Env1 = push_clauses_controls(Env, #clauses_controls{exhaust = Env#env.exhaust}),
    %% This is fine, since we match on `any' in the clause above.
    ArgsTy = ?assert_type(ArgsTy, [type()]),
    %% Clauses for if, case, functions, receive, etc.
    {VarBindsList, Css, RefinedArgsTy, Env2} =
        lists:foldl(fun (Clause, {VBs, Css, RefinedArgsTy, EnvIn}) ->
                            {NewRefinedArgsTy, Env2, Cs} =
                                check_clause(EnvIn, RefinedArgsTy, ResTy, Clause, Caps),
                            VB =
                                refine_vars_by_mismatching_clause(Clause, EnvIn#env.venv, Env2),
                            {[Env2 | VBs],
                             [Cs | Css],
                             NewRefinedArgsTy,
                             Env2#env{venv = VB}}
                    end,
                    {[], [], ArgsTy, Env1},
                    Clauses),
    check_arg_exhaustiveness(Env2, ArgsTy, Clauses, RefinedArgsTy),
    Env3 = pop_clauses_controls(Env2),
    {union_var_binds(VarBindsList, Env3), constraints:combine(Css)}.

push_clauses_controls(#env{} = Env, #clauses_controls{} = CC) ->
    ?verbose(Env, "Pushing ~p~n", [CC]),
    CStack = Env#env.clauses_stack,
    Env#env{clauses_stack = [CC | CStack]}.

pop_clauses_controls(#env{} = Env) ->
    ?verbose(Env, "Popping clauses controls~n", []),
    [_ | CStack] = Env#env.clauses_stack,
    Env#env{clauses_stack = CStack}.

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
check_arg_exhaustiveness(Env, ArgTys, Clauses, RefinedArgTys) ->
    case exhaustiveness_checking(Env) andalso
         all_refinable(ArgTys, Env) andalso
         no_clause_has_guards(Clauses) andalso
         some_type_not_none(RefinedArgTys)
    of
        true ->
            [{clause, P, _, _, _} | _] = Clauses,
            throw(nonexhaustive(P, gradualizer_lib:pick_value(RefinedArgTys, Env)));
        _ ->
            ok
    end.

exhaustiveness_checking(#env{} = Env) ->
    [#clauses_controls{} = B | _] = Env#env.clauses_stack,
    Exhaust = B#clauses_controls.exhaust,
    ?verbose(Env, "Exhaustiveness checking: ~p~n", [Exhaust]),
    Exhaust.

all_refinable(any, _Env) -> false;
all_refinable(Types, Env) -> lists:all(fun (Ty) -> refinable(Ty, Env) end, Types).

no_clause_has_guards(Clauses) ->
    lists:all(fun no_guards/1, Clauses).

some_type_not_none(Types) when is_list(Types) ->
    lists:any(fun (T) -> T =/= type(none) end, Types).

%% This function checks clauses.
%% * If clauses have 0 arguments;
%% * case/try/catch/receive clauses have 1 argument;
%% * function clauses have any number of arguments;
%% * the patterns for catch C:E:T is represented as {C,E,T}
-spec check_clause(env(), [type()], type(), gradualizer_type:abstract_clause(),
		   capture_vars | bind_vars) ->
        {RefinedTys :: [type()] , VarBinds :: env(), constraints:constraints()}.
check_clause(_Env, [?type(none)|_], _ResTy, {clause, P, _Args, _Guards, _Block}, _) ->
    throw(type_error(unreachable_clause, P));
check_clause(Env, ArgsTy, ResTy, C = {clause, P, Args, Guards, Block}, Caps) ->
    ?verbose(Env, "~sChecking clause :: ~s~n", [gradualizer_fmt:format_location(C, brief), typelib:pp_type(ResTy)]),
    case {length(ArgsTy), length(Args)} of
        {L, L} ->
            {PatTys, _UBounds, EnvNew, Cs1} = add_types_pats(Args, ArgsTy, Env, Caps),
            VarBinds1   = check_guards(EnvNew, Guards),
            EnvNewest   = add_var_binds(EnvNew, VarBinds1, EnvNew),
            {VarBinds2, Cs2} = type_check_block_in(EnvNewest, ResTy, Block),
            RefinedTys1 = refine_clause_arg_tys(ArgsTy, PatTys,
                                                Guards, EnvNewest),
            RefinedTys2 = refine_mismatch_using_guards(RefinedTys1, C,
                                                       EnvNewest#env.venv, EnvNewest),
            {RefinedTys2
            ,union_var_binds([VarBinds1, VarBinds2, EnvNewest], EnvNewest)
            ,constraints:combine(Cs1, Cs2)};
        {LenTy, LenArgs} ->
            LenTy = ?assert_type(LenTy, arity()),
            LenArgs = ?assert_type(LenArgs, arity()),
            throw(argument_length_mismatch(P, LenTy, LenArgs))
    end.
%% DEBUG
%check_clause(_Env, _ArgsTy, _ResTy, Term, _) ->
%    io:format("DEBUG: check_clause term: ~p~n", [Term]),
%    throw(check_clause).

%% Refine types by matching clause. MatchedTys are the types exhausted by
%% each pattern in the previous clause.
-spec refine_clause_arg_tys([type()], [type()], _Guards, env()) -> [type()].
refine_clause_arg_tys(Tys, MatchedTys, [], Env) ->
    Ty        = type(tuple, Tys),
    MatchedTy = type(tuple, MatchedTys),
    case type_diff(Ty, MatchedTy, Env) of
        ?type(tuple, RefTys) ->
            RefTys;
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
                              [[{call, _, {atom, _, Fun}, Args = [{var, _, Var}]}]],
                              _Block},
                             VEnv, Env) ->
    %% A single guard on the form `when is_TYPE(Var)' where Var is one of the
    %% patterns matched against.  If Var was free before the clause (i.e. it
    %% becomes bound in the clause), which failed because of a failing is_TYPE
    %% guard, we can refine the type of that *pattern*.
    PatternCantFail = are_patterns_matching_all_input(Pats, VEnv),
    case {maps:is_key(Var, VEnv), check_guard_call(Fun, Args)} of
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
    end;
refine_mismatch_using_guards(PatTys, {clause, _, _, _, _}, _VEnv, _Env) ->
    %% No Refinement
    PatTys.

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
refine(OrigTy, Ty, Trace, Env) ->
    %% If we're being called recursively and see OrigTy again throw no_refinement.
    %% This is a safeguard similar to the mutual recurson of glb/glb_ty,
    %% or to the stop_refinable_recursion loop breaker.
    case maps:is_key(OrigTy, Trace) of
        true ->
            throw(no_refinement);
        false ->
            NormTy = normalize(OrigTy, Env),
            case refine_ty(NormTy, normalize(Ty, Env), maps:put(OrigTy, {}, Trace), Env) of
                NormTy -> OrigTy;
                RefTy  -> RefTy
            end
    end.

-spec get_record_fields_types(atom(), erl_anno:anno(), env()) -> [_].
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
refine_ty(?type(record, [Name|FieldTys1]), ?type(record, [Name|FieldTys2]), Trace, Env)
  when length(FieldTys1) > 0, length(FieldTys1) == length(FieldTys2) ->
    % Record without just the name
    Tys1 = [Ty || ?type_field_type(_, Ty) <- FieldTys1],
    Tys2 = [Ty || ?type_field_type(_, Ty) <- FieldTys2],
    RefTys = [refine(Ty1, Ty2, Trace, Env) || {Ty1, Ty2} <- lists:zip(Tys1, Tys2)],
    RecordsTys = pick_one_refinement_each(Tys1, RefTys),
    RecordsElems = [ [ type_field_type(FieldName, RecordTy) || {?type_field_type(FieldName, _), RecordTy} <- lists:zip(FieldTys1, RecordTys)]
        || RecordTys <- RecordsTys],
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
    lists:foldl(fun (UnionTy, AccTy) ->
                        try refine(AccTy, UnionTy, Trace, Env) of
                            RefTy -> RefTy
                        catch
                            disjoint -> AccTy
                        end
                end,
                Ty,
                UnionTys);
refine_ty(?type(map, _Assocs), ?type(map, [?any_assoc]), _Trace, _Env) ->
    %% #{x => y} \ map() = none()
    type(none);
refine_ty(?type(map, Assocs1) = Ty1, ?type(map, Assocs2) = Ty2, _Trace, Env) ->
    case {has_overlapping_keys(Ty1, Env), has_overlapping_keys(Ty2, Env)} of
        {false, false} ->
            FieldTys = lists:flatmap(fun refine_map_field_ty/1,
                                     [ {As1, As2} || As1 <- Assocs1, As2 <- Assocs2 ]),
            case lists:partition(fun (Ty) -> Ty == type(map, []) end, FieldTys) of
                {[_|_], []} ->
                    %% All empty map types mean an empty map type should be returned,
                    %% since there are no meaningful fields in the map anymore.
                    type(map, []);
                {_, RemainingFields} ->
                    %% If apart from the empty map types there are also other fields present,
                    %% make sure no more than one copy of each is returned in the final type.
                    RemainingDeduped = deduplicate_list(RemainingFields),
                    length(RemainingDeduped) =< length(Assocs1) orelse erlang:error(unreachable),
                    type(map, RemainingDeduped)
            end;
        _ ->
            throw(no_refinement)
    end;
refine_ty(?type(tuple, _Tys), ?type(tuple, any), _Trace, _Env) ->
    %% {x,y,z} \ tuple() = none()
    type(none);
refine_ty(?type(tuple, Tys1), ?type(tuple, Tys2), Trace, Env)
  when length(Tys1) > 0, length(Tys1) == length(Tys2) ->
    %% Non-empty tuple
    RefTys = [refine(Ty1, Ty2, Trace, Env) || {Ty1, Ty2} <- lists:zip(Tys1, Tys2)],
    %% {a|b, a|b} \ {a,a} => {b, a|b}, {a|b, b}
    TuplesElems = pick_one_refinement_each(Tys1, RefTys),
    Tuples = [type(tuple, TupleElems) || TupleElems <- TuplesElems],
    normalize(type(union, Tuples), Env);
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
    case glb(Ty1, Ty2, Env) of
        {?type(none), _} -> throw(disjoint);  %% disjoint
        _NotDisjoint -> throw(no_refinement)  %% imprecision
    end.

-spec refine_map_field_ty({_, _}) -> [gradualizer_type:abstract_type()].
%% For the same key K in both M1 and M2 the diff over its field (ignoring other keys) is:
%%
%% M1 \ M2    | #{K := V} | #{K => V}
%% -----------+-----------+--------------
%% #{K := V}  | none      | #{K => V}
%% #{K => V}  | #{}       | none
%%
%% Please note that we're changing field optionality, but not the types of keys or values.
%%
%% We might also see disjoint K1 from M1 and K2 from M2.
%% In such a case we just leave K1 and its field unmodified.
refine_map_field_ty({?type(map_field_exact, KVTy), ?type(map_field_exact, KVTy)}) ->
    [];
refine_map_field_ty({?type(map_field_assoc, KVTy), ?type(map_field_assoc, KVTy)}) ->
    [];
refine_map_field_ty({?type(map_field_assoc, KVTy), ?type(map_field_exact, KVTy)}) ->
    %% #{x => y} \ #{x := y} = #{} -- i.e. an empty map
    [type(map, [])];
refine_map_field_ty({?type(map_field_exact, KVTy), ?type(map_field_assoc, KVTy)}) ->
    %% M1 = #{x := y}
    %% M2 = #{x => y}
    %% M1 \ M2 = #{x => y}
    [type(map_field_assoc, KVTy)];
refine_map_field_ty({?type(AssocTag1, _) = Assoc1, ?type(AssocTag2, _)})
  when AssocTag1 == map_field_assoc, AssocTag2 == map_field_assoc;
       AssocTag1 == map_field_exact, AssocTag2 == map_field_exact;
       AssocTag1 == map_field_exact, AssocTag2 == map_field_assoc;
       AssocTag1 == map_field_assoc, AssocTag2 == map_field_exact ->
    %% TODO: this might lead to duplicates in the resulting type!
    [Assoc1].

deduplicate_list(List) ->
    {L, _} = lists:foldl(fun(Elem, {LAcc, SAcc}) ->
                                 case maps:is_key(Elem, SAcc) of
                                     true ->
                                         {LAcc, SAcc};
                                     false ->
                                         {[Elem | LAcc], maps:put(Elem, {}, SAcc)}
                                 end
                         end, {[], #{}}, List),
    lists:reverse(L).

%% Returns a nested list on the form
%%
%%     [[RefTy1, Ty2, Ty3, ..., TyN],
%%      [Ty1, RefTy2, Ty3, ..., TyN],
%%      [Ty1, Ty2, RefTy3, ..., TyN],
%%      ...
%%      [Ty1, Ty2, Ty3, ..., RefTyN]].
%%
%% If RefTyI == none() for any I, that list I is excluded.
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
    RefTailCombinations =
        [[Ty|Tail] || Tail <- pick_one_refinement_each(Tys, RefTys)],
    %% The last list is the list where to type is refined.
    RefHeadCombinations ++ RefTailCombinations.

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
    case stop_refinable_recursion(Ty, Env, Trace) of
        stop -> true;
        {proceed, NewTrace} ->
            case has_overlapping_keys(Ty, Env) of
                true ->
                    false;
                false ->
                    lists:all(fun ({type, _, _AssocTag, [KTy, VTy]}) ->
                                      refinable(KTy, Env, NewTrace)
                                      andalso
                                      refinable(VTy, Env, NewTrace)
                              end, Assocs)
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

no_guards({clause, _, _, Guards, _}) ->
    Guards == [].

%% Refines the types of bound variables using the assumption that a clause has
%% mismatched.
-spec refine_vars_by_mismatching_clause(_Clause, VEnv, env()) -> VEnv.
refine_vars_by_mismatching_clause({clause, _, Pats, Guards, _Block}, VEnv, Env) ->
    PatternCantFail = are_patterns_matching_all_input(Pats, VEnv),
    case Guards of
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
            %% No refinement
            VEnv
    end.

%% Returns true if a list of patterns match all possible inputs of the same
%% length. For this to be true, all patterns must be distinct free variables or
%% the underscore pattern.
%%
%% See also `should_list_pat_enable_exhaustiveness_check/1', which is similar,
%% but incomplete and specific to list patterns.
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
mirror_comp_op('<')  -> '>';
mirror_comp_op('>')  -> '<';
mirror_comp_op('>=') -> '=<';
mirror_comp_op('=<') -> '>=';
mirror_comp_op(Comm) -> Comm.

-spec check_guard_expression(env(), term()) -> env().
check_guard_expression(Env, {call, _, {atom, _, Fun}, Vars}) ->
    Env#env{venv = check_guard_call(Fun, Vars)};
check_guard_expression(Env, {call, _, {remote,_, {atom, _, erlang},{atom, _, Fun}}, Vars}) ->
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
    {_Ty, Env1, _Cs} = type_check_expr(Env, Guard), % Do we need to thread the Env?
    Env1.

%% The different guards use glb
-spec check_guard(env(), list()) -> env().
check_guard(Env, GuardSeq) ->
    RefTys = union_var_binds(
               lists:map(fun (Guard) ->
                                 check_guard_expression(Env, Guard)
                         end, GuardSeq),
               Env),
    Env#env{venv = maps:merge(Env#env.venv, RefTys#env.venv)}.

%% TODO: implement proper checking of guards.
-spec check_guards(env(), list()) -> env().
check_guards(Env, []) -> Env;
check_guards(Env, Guards) ->
    Envs = lists:map(fun (Guard) ->
                             check_guard(Env, Guard)
                     end, Guards),
    union_var_binds_symmetrical(Envs, Env).

-spec type_check_function(env(), expr()) -> {env(), constraints:constraints()}.
type_check_function(Env, {function, _, Name, NArgs, Clauses}) ->
    ?verbose(Env, "Checking function ~p/~p~n", [Name, NArgs]),
    case maps:find({Name, NArgs}, Env#env.fenv) of
        {ok, FunTy} ->
            NewEnv = Env#env{current_spec = FunTy},
            %% TODO: https://github.com/josefs/Gradualizer/issues/388
            FunTyNoPos = case FunTy of
                             _ when is_list(FunTy) ->
                                 [ typelib:remove_pos(Ty) || Ty <- FunTy ];
                             _ ->
                                 typelib:remove_pos(FunTy)
                         end,
            check_clauses_fun(NewEnv, expect_fun_type(NewEnv, FunTyNoPos), Clauses);
        error ->
            throw(internal_error(missing_type_spec, Name, NArgs))
    end.

-spec position_info_from_spec(form() | forms() | none) -> erl_anno:anno().
position_info_from_spec(none) ->
    %% This simplifies testing internal functions.
    %% In these cases we don't go through type_check_function,
    %% but call deeper into the typechecker directly.
    erl_anno:new(0);
position_info_from_spec([_|_] = Forms) ->
    %% TODO: https://github.com/josefs/Gradualizer/issues/388
    position_info_from_spec(hd(Forms));
position_info_from_spec(Form) ->
    Form = ?assert_type(Form, form()),
    element(2, Form).

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
            NewEnv      :: env(),
            Constraints :: constraints:constraints()}.
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
            NewEnv      :: env(),
            Constraints :: constraints:constraints()}.
do_add_types_pats(Pats, Tys, Env) ->
    add_types_pats(Pats, Tys, Env, [], [], []).

%% TODO: move tenv to back
-spec add_types_pats(Pats, Tys, Env, PatTysAcc, UBoundsAcc, CsAcc) -> R when
      Pats       :: [gradualizer_type:abstract_pattern()],
      Tys        :: [type()],
      Env        :: env(),
      PatTysAcc  :: [type()],
      UBoundsAcc :: [type()],
      CsAcc      :: [constraints:constraints()],
      R :: {PatTys      :: [type()],
            UBounds     :: [type()],
            NewEnv      :: env(),
            Constraints :: constraints:constraints()}.
add_types_pats([], [], Env, PatTysAcc, UBoundsAcc, CsAcc) ->
    {lists:reverse(PatTysAcc), lists:reverse(UBoundsAcc), Env, constraints:combine(CsAcc)};
add_types_pats([Pat | Pats], [Ty | Tys], Env, PatTysAcc, UBoundsAcc, CsAcc) ->
    NormTy = normalize(Ty, Env),
    {PatTyNorm, UBoundNorm, Env2, Cs1} =
        ?throw_orig_type(add_type_pat(Pat, NormTy, Env), Ty, NormTy),
    %% De-normalize the returned types if they are the type checked against.
    PatTy  = denormalize(Ty, PatTyNorm, NormTy),
    UBound = denormalize(Ty, UBoundNorm, NormTy),
    add_types_pats(Pats, Tys, Env2, [PatTy|PatTysAcc], [UBound|UBoundsAcc], [Cs1|CsAcc]).

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
            NewEnv :: env(),
            constraints:constraints()}.
add_type_pat({var, _, '_'}, Ty, Env) ->
    {Ty, Ty, Env, constraints:empty()};
add_type_pat({var, _, A} = Var, Ty, Env) ->
    case Env#env.venv of
        #{A := VarTy} ->
            case glb(VarTy, Ty, Env) of
                {?type(none), _Cs} ->
                    %% TODO: Better type error (it's a pattern, not an expression)
                    throw(type_error(Var, VarTy, Ty));
                {RefinedTy, Cs} ->
                    {type(none), RefinedTy, update_var_type(Env, A, RefinedTy), Cs}
            end;
        _FreeVar ->
            %% Match all
            {Ty, Ty, set_var_type(Env, A, Ty), constraints:empty()}
    end;
add_type_pat(Pat, ?type(union, UnionTys) = UnionTy, Env) ->
    {PatTys, UBounds, Envs, Css} =
        lists:foldr(fun (Ty, {PatTysAcc, UBoundsAcc, EnvAcc, CsAcc} = Acc) ->
                        %% Ty is normalized, since UnionTy is normalized
                        try add_type_pat(Pat, Ty, Env) of
                            {PatTy, UBound, NewEnv, Cs} ->
                                {[PatTy|PatTysAcc],
                                 [UBound|UBoundsAcc],
                                 [NewEnv|EnvAcc],
                                 [Cs|CsAcc]}
                        catch _TypeError ->
                            Acc
                        end
                    end,
                    {[], [], [], []},
                    UnionTys),
    case PatTys of
        [] ->
            %% Pattern doesn't match any type in the union
            Anno = element(2, Pat),
            throw(type_error(pattern, Anno, Pat, UnionTy));
        _SomeTysMatched ->
            %% TODO: The constraints should be merged with *or* semantics
            %%       and var binds with intersection
            {Ty, Cs} = glb(PatTys, Env),
            {Ty,
             normalize(type(union, UBounds), Env),
             union_var_binds(Envs, Env),
             constraints:combine([Cs|Css])}
    end;
add_type_pat(Lit = {Tag, P, Val}, Ty, Env)
  when Tag =:= integer;
       Tag =:= char ->
    LitTy = {integer, erl_anno:new(0), Val},
    case subtype(LitTy, Ty, Env) of
        {true, Cs} ->
            {LitTy, LitTy, Env, Cs};
        false ->
            throw(type_error(pattern, P, Lit, Ty))
    end;
add_type_pat(Lit = {float, P, _}, Ty, Env) ->
    case subtype(type(float), Ty, Env) of
        {true, Cs} ->
            {type(none), type(float), Env, Cs};
        false ->
            throw(type_error(pattern, P, Lit, Ty))
    end;
add_type_pat(Tuple = {tuple, P, Pats}, Ty, Env) ->
    case expect_tuple_type(Ty, length(Pats)) of
        any ->
            NewEnv = union_var_binds([ add_any_types_pat(Pat, Env) || Pat <- Pats ], Env),
            {type(none),
             Ty,
             NewEnv,
             constraints:empty()};
        {elem_ty, Tys, Cs} ->
            {PatTys, UBounds, Env1, Cs1} = do_add_types_pats(Pats, Tys, Env),
            {type(tuple, PatTys),
             type(tuple, UBounds),
             Env1,
             constraints:combine(Cs, Cs1)};
        {type_error, _Type} ->
            throw(type_error(pattern, P, Tuple, Ty))
    end;
add_type_pat(Atom = {atom, P, Val}, Ty, Env) ->
    LitTy = {atom, erl_anno:new(0), Val},
    case subtype(LitTy, Ty, Env) of
        {true, Cs} ->
            {LitTy, LitTy, Env, Cs};
        false ->
            throw(type_error(pattern, P, Atom, Ty))
    end;
add_type_pat(Nil = {nil, P}, Ty, Env) ->
    NilTy = type(nil),
    case subtype(NilTy, Ty, Env) of
        {true, Cs} ->
            {NilTy, NilTy, Env, Cs};
        false ->
            throw(type_error(pattern, P, Nil, Ty))
    end;
add_type_pat(CONS = {cons, P, PH, PT}, ListTy, Env) ->
    case expect_list_type(normalize(ListTy, Env), dont_allow_nil_type, Env) of
        any ->
            Env2 = add_any_types_pat(PH, Env),
            TailTy = normalize(type(union, [ListTy, type(nil)]), Env),
            {_TailPatTy, _TauUBound, VEnv3, Cs} = add_type_pat(PT, TailTy, Env2),
            NonEmptyTy = rewrite_list_to_nonempty_list(ListTy),
            {type(none), NonEmptyTy, VEnv3, Cs};
        {elem_ty, ElemTy, Cs1} ->
            {PatTy1, _UBound1, Env2, Cs2} =
                add_type_pat(PH, normalize(ElemTy, Env), Env),
            TailTy = normalize(type(union, [ListTy, type(nil)]), Env),
            {_PatTy2, _Ubound2, Env3, Cs3} = add_type_pat(PT, TailTy, Env2),
            {PatTy, Env4} = case should_list_pat_enable_exhaustiveness_check(CONS) of
                                true ->
                                    {type(nonempty_list, [PatTy1]), Env3};
                                false ->
                                    {type(none), disable_exhaustiveness_check(Env3)}
                            end,
            NonEmptyTy = rewrite_list_to_nonempty_list(ListTy),
            {PatTy, NonEmptyTy, Env4, constraints:combine([Cs1, Cs2, Cs3])};
        {type_error, _Ty} ->
            throw(type_error(cons_pat, P, CONS, ListTy))
    end;
add_type_pat(String = {string, P, _}, Ty, Env) ->
    case subtype(type(string), Ty, Env) of
        {true, Cs} ->
            {type(none), normalize(type(string), Env), Env, Cs};
        false ->
            throw(type_error(pattern, P, String, Ty))
    end;
add_type_pat({bin, _P, BinElements} = Bin, Ty, Env) ->
    %% Check the size parameters of the bit pattern
    BinTy = gradualizer_bin:compute_type(Bin),
    Cs1 = case subtype(BinTy, Ty, Env) of
              {true, Cs0} ->
                  Cs0;
              false ->
                  throw(type_error(Bin, BinTy, Ty))
          end,
    %% Check the elements
    {Env2, Cs} =
        lists:foldl(fun ({bin_element, _, Pat, _Size, _Specifiers} = BinElem,
                         {EnvAcc, CsAcc}) ->
                            %% Check Pat against the bit syntax type specifiers
                            ElemTy = type_of_bin_element(BinElem, pattern),
                            {_PatTy, _UBound, Env2, Cs2} =
                                add_type_pat(Pat, ElemTy, EnvAcc),
                            {Env2, constraints:combine(CsAcc, Cs2)}
                    end,
                    {Env, Cs1},
                    BinElements),
    {PatTy, Env3} = case should_bin_pat_enable_exhaustiveness_check(BinElements) of
                        true ->
                            {BinTy, Env2};
                        false ->
                            {type(none), disable_exhaustiveness_check(Env2)}
                    end,
    {PatTy, BinTy, Env3, Cs};
add_type_pat({record, P, Record, Fields}, Ty, Env) ->
    case expect_record_type(Ty, Record, Env) of
        any ->
            NewEnv = union_var_binds([Env] ++ [ add_any_types_pat(Field, Env)
                                                || ?record_field_expr(Field) <- Fields ], Env),
            {type(none), Ty,
             NewEnv,
             constraints:empty()};
        {fields_ty, Tys, Cs} ->
            {PatTys, UBounds, Env1, Cs1} = add_type_pat_fields(Fields, Tys, Env),
            {type_record(Record, PatTys),
             type_record(Record, UBounds),
             Env1,
             constraints:combine(Cs, Cs1)};
        {type_error, _Type} ->
            throw(type_error(record_pattern, P, Record, Ty))
    end;
add_type_pat({map, P, AssocPats} = MapPat, MapTy, Env) ->
    NormMapTy = normalize(MapTy, Env),
    case expect_map_type(NormMapTy, Env) of
        any ->
            NewEnv = add_any_types_pat(MapPat, Env),
            {type(none), type(any), NewEnv, constraints:empty()};
        {assoc_tys, AssocTys, Cs0} ->
            AssocTys = ?assert_type(AssocTys, [gradualizer_type:af_assoc_type()]),
            %% Check each Key := Value and bind vars in Value.
            {NewEnv, Css} =
                lists:foldl(fun ({map_field_exact, _, Key, ValuePat}, {EnvIn, CsAcc}) ->
                                    case add_type_pat_map_key(Key, AssocTys, EnvIn) of
                                        {ok, ValueTy, Cs1} ->
                                            {_ValPatTy, _ValUBound, EnvOut, Cs2} =
                                                add_type_pat(ValuePat, normalize(ValueTy, EnvIn), EnvIn),
                                            {EnvOut, [Cs1, Cs2 | CsAcc]};
                                        error ->
                                            throw(type_error(badkey, Key, MapTy))
                                    end
                            end,
                            {Env, [Cs0]}, AssocPats),
                PatTy = case NormMapTy of
                            ?top() ->
                                top();
                            {var, _, _Var} ->
                                type(none);
                            ?type(map, Assocs) when is_list(Assocs) ->
                                rewrite_map_assocs_to_exacts(NormMapTy)
                        end,
            {PatTy, MapTy, NewEnv, constraints:combine(Css)};
        {type_error, _Type} ->
            throw(type_error(pattern, P, MapPat, MapTy))
    end;
add_type_pat({match, _, {var, _, _Var} = PatVar, Pat}, Ty, Env) ->
    add_type_pat_var(Pat, PatVar, Ty, Env);
add_type_pat({match, _, Pat, {var, _, _Var} = PatVar}, Ty, Env) ->
    add_type_pat_var(Pat, PatVar, Ty, Env);
add_type_pat({match, _, Pat1, Pat2}, Ty, Env) ->
    %% Use the refined type of Pat2 to bind vars in Pat1.
    {PatTy1, Ty1, Env1, Cs1} = add_type_pat(Pat2, Ty, Env),
    {PatTy2, Ty2, Env2, Cs2} = add_type_pat(Pat1, Ty1, Env1),
    {GlbTy, Cs3} = glb(PatTy1, PatTy2, Env),
    {GlbTy, Ty2, Env2, constraints:combine([Cs1, Cs2, Cs3])};
add_type_pat({op, _, '++', Pat1, Pat2}, Ty, Env) ->
    {_, _, Env1, Cs1} = add_type_pat(Pat1, Ty, Env),
    {_, _, Env2, Cs2} = add_type_pat(Pat2, Ty, Env1),
    {type(none), Ty, Env2, constraints:combine(Cs1,Cs2)};
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

%% TODO: This is incomplete!
%% To properly check pattern exhaustiveness we have to consider bound variables.
%%
%% See also `are_patterns_matching_all_input/2',
%% which is similar, but limited to variable patterns.
should_list_pat_enable_exhaustiveness_check({nil, _}) -> true;
should_list_pat_enable_exhaustiveness_check({cons, _, {var, _, _}, {var, _, _}}) -> true;
should_list_pat_enable_exhaustiveness_check(_) -> false.

%% TODO: This is incomplete!
%% To properly check pattern exhaustiveness we have to consider bound variables.
%% Pattern: <<>>
should_bin_pat_enable_exhaustiveness_check([]) ->
    true;
%% Pattern: <<_, _/bytes>>
should_bin_pat_enable_exhaustiveness_check([{bin_element, _, {var, _, _}, _, _},
                       {bin_element, _, {var, _, _}, _, [Bytes]}])
  when Bytes =:= bytes; Bytes =:= binary ->
    true;
should_bin_pat_enable_exhaustiveness_check(_) ->
    false.

-spec expect_map_type(type(), env()) -> R when
      R :: any
         | {assoc_tys, [type()] | any, constraints:constraints()}
         | {type_error, type()}.
expect_map_type(?type(any), _Env) ->
    any;
expect_map_type(?top(), _Env) ->
    {assoc_tys, [type(map_field_assoc, [top(), top()])], constraints:empty()};
expect_map_type({var, _, Var}, _Env) ->
    %% FIXME this is a quite rudimentary implementation
    %% - variables from the map pattern become any()
    %% - the constraint could contain the map keys
    Cs = constraints:add_var(Var, constraints:upper(Var, type(map, any))),
    {assoc_tys, any, Cs};
expect_map_type(?type(map, AssocTys), _Env) ->
    {assoc_tys, AssocTys, constraints:empty()};
expect_map_type(Ty, _Env) ->
    {type_error, Ty}.

%% Rewrite map_field_assoc to map_field_exact to return in pattern types.
%%
%% Similarly to map field type inference on map creation - if a pattern matches,
%% then the map field is exact (:=), not assoc (=>).
%% There isn't even syntax for optional fields in map patterns.
rewrite_map_assocs_to_exacts(?type(map, Assocs)) ->
    type(map, lists:map(fun ({type, Ann, _, KVTy}) ->
                                {type, Ann, map_field_exact, KVTy}
                        end, Assocs)).

-spec add_type_pat_var(_, _, type(), env()) -> any().
add_type_pat_var(Pat, PatVar, Ty, Env) ->
    %% Refine using Pat1 first to be able to bind Pat2 to a refined type.
    {PatTy1, Ty1, Env1, Cs2} = add_type_pat(Pat, Ty, Env),
    {PatTy2, Ty2, Env2, Cs1} = add_type_pat(PatVar, Ty1, Env1),
    {GlbTy, Cs3} = glb(PatTy1, PatTy2, Env),
    {GlbTy, Ty2, Env2, constraints:combine([Cs1, Cs2, Cs3])}.

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

find_field_or_create([], Name, Default) -> {record_field, erl_anno:new(0), {atom, erl_anno:new(0), Name}, Default};
find_field_or_create([Field = ?record_field(Name)|_], Name, _Default) -> Field;
find_field_or_create([_|Fields], Name, Default) -> find_field_or_create(Fields, Name, Default).

find_field_default([]) -> {var, erl_anno:new(0), '_'};
find_field_default([{record_field, _, {var, _, '_'}, Exp}|_]) -> Exp;
find_field_default([_|Fields]) -> find_field_default(Fields).

-spec add_type_pat_fields(_, _, env()) -> any().
add_type_pat_fields([], _, Env) ->
    {[], [], Env, constraints:empty()};
add_type_pat_fields(Fields, Tys, Env) ->
    %% Add every missing fields
    %% If an underscore field is present: use that expression as the default expression
    %% Otherwise, give an empty assignment of the field to underscore
    Default = find_field_default(Fields),
    AllFields = [ find_field_or_create(Fields, Name, Default) || ?typed_record_field(Name) <- Tys],
    add_type_pat_fields(AllFields, Tys, Env, [], [], []).

-spec add_type_pat_fields(_, _, env(), _, _, _) -> any().
add_type_pat_fields([], _, Env, PatTysAcc, UBoundsAcc, CsAcc) ->
    {lists:reverse(PatTysAcc), lists:reverse(UBoundsAcc),
     Env, constraints:combine(CsAcc)};
add_type_pat_fields([{record_field, _, {atom, _, Name} = FieldWithAnno, Pat}|Fields],
                    Record, Env, PatTysAcc, UBoundsAcc, CsAcc) ->
    Ty = get_rec_field_type(FieldWithAnno, Record),
    NormTy = normalize(Ty, Env),
    {PatTyNorm, UBoundNorm, Env2, Cs1} =
        ?throw_orig_type(add_type_pat(Pat, NormTy, Env), Ty, NormTy),
    %% De-normalize the returned types if they are the type checked against.
    RawPatTy  = case PatTyNorm  of NormTy -> Ty;
                                _      -> PatTyNorm end,
    PatTy = type_field_type(Name, RawPatTy),
    RawUBound = case UBoundNorm of NormTy -> Ty;
                                _      -> UBoundNorm end,
    UBound = type_field_type(Name, RawUBound),
    add_type_pat_fields(Fields, Record, Env2, [PatTy|PatTysAcc], [UBound|UBoundsAcc], [Cs1|CsAcc]).

%% Given a pattern for a key, finds the matching association in the map type and
%% returns the value type. Returns 'error' if the key is not valid in the map.
-spec add_type_pat_map_key(Key         :: gradualizer_type:abstract_pattern(),
                           MapTyAssocs :: any | [gradualizer_type:af_assoc_type()],
                           Env         :: env()
                          ) -> {ok, ValueTy :: type(), constraints:constraints()} |
                               error.
add_type_pat_map_key(_Key, any, _Env) ->
    {ok, type(any), constraints:empty()};
add_type_pat_map_key(Key, [{type, _, AssocTag, [KeyTy, ValueTy]} | MapAssocs], Env)
  when AssocTag == map_field_exact; AssocTag == map_field_assoc ->
    try add_types_pats([Key], [KeyTy], Env, capture_vars) of
        {_, _, _VEnv, Cs} ->
            %% No free vars in Key, so no new variable binds.  (Types in VEnv
            %% can be refined though, so _VEnv doesn't have to match VEenv.)
            {ok, ValueTy, Cs}
    catch _TypeError ->
        add_type_pat_map_key(Key, MapAssocs, Env)
    end;
add_type_pat_map_key(_Key, [], _Env) ->
    %% Key is not defined in this map type.
    error.

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
                                {true, {type, erl_anno:new(0), binary,
                                        [{integer, erl_anno:new(0), 0}
                                        ,{integer, erl_anno:new(0), 8}]}};
                            (S) when S == bitstring; S == bits ->
                                %% TODO: Consider Size and Unit
                                {true, {type, erl_anno:new(0), binary,
                                        [{integer, erl_anno:new(0), 0}
                                        ,{integer, erl_anno:new(0), 1}]}};
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
            T
    end.


%%% Helper functions

-spec type(map, any) -> type();
          (tuple, any) -> type();
          (atom(), [any()]) -> type().
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

-spec top() -> type().
top() ->
    {remote_type, erl_anno:new(0), [{atom, erl_anno:new(0), gradualizer}
				   ,{atom, erl_anno:new(0), top},[]]}.

type_var(Name) ->
    {var, erl_anno:new(0), Name}.

type_record(Name) ->
    type_record(Name, []).

type_record(Name, Fields) ->
    {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Name} | Fields]}.

type_field_type(Name, Type) ->
    {type, erl_anno:new(0), field_type, [{atom, erl_anno:new(0), Name}, Type]}.

type_fun(Arity) ->
    Args = [{type, erl_anno:new(0), any, []} || _ <- lists:seq(1, Arity)],
    {type, erl_anno:new(0), 'fun', [{type, erl_anno:new(0), product, Args}, {type, erl_anno:new(0), any, []}]}.

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
    % TODO: Don't drop the constraints
    Glb = fun(_K, Ty1, Ty2) -> {Ty, _Cs} = glb(Ty1, Ty2, Env), Ty end,
    Env#env{venv = union_var_binds_help([ E#env.venv || E <- Envs ], Glb)}.

%% Tail recursive helper.
-spec union_var_binds_help([venv()], _) -> venv().
union_var_binds_help([#{} = VB1, #{} = VB2 | Rest], Glb) ->
    VB = gradualizer_lib:merge_with(Glb, VB1, VB2),
    union_var_binds_help([VB | Rest], Glb);
union_var_binds_help([#{} = VB], _) -> VB.

-spec add_var_binds(env(), env(), env()) -> env().
add_var_binds(#env{venv = VB1}, #env{venv = VB2}, #env{} = Env) ->
    % TODO: Don't drop the constraints
    Glb = fun(_K, Ty1, Ty2) -> {Ty, _C} = glb(Ty1, Ty2, Env), Ty end,
    Env#env{venv = gradualizer_lib:merge_with(Glb, VB1, VB2)}.

%% Set the type of a new variable.
-spec set_var_type(env(), atom() | string(), type()) -> env().
set_var_type(Env, A, Ty) ->
    VEnv = Env#env.venv,
    Env#env{venv = VEnv#{A => Ty}}.

%% Update the type of an already seen variable.
-spec update_var_type(env(), atom() | string(), type()) -> env().
update_var_type(Env, A, Ty) ->
    VEnv = Env#env.venv,
    Env#env{venv = VEnv#{A := Ty}}.

%% From a record field name, find the default value from the typed list of record field definitions
get_rec_field_default({atom, _, FieldName},
                    [{typed_record_field,
                        {record_field, _, {atom, _, FieldName}, Default}, _}|_]) ->
    Default;
get_rec_field_default(FieldWithAnno, [_|RecFields]) ->
    get_rec_field_default(FieldWithAnno, RecFields);
get_rec_field_default(FieldWithAnno, []) ->
    throw(undef(record_field, FieldWithAnno)).

get_rec_field_type(FieldWithAnno, RecFields) ->
    %% The first field is the second element of the tuple - so start from 2
    {_Index, Ty} = get_rec_field_index_and_type(FieldWithAnno, RecFields, 2),
    Ty.

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

get_rec_field_index(FieldWithAnno, RecFields) ->
    %% The first field is the second element of the tuple - so start from 2
    {Index, _Ty} = get_rec_field_index_and_type(FieldWithAnno, RecFields, 2),
    Index.

get_rec_field_index_and_type({atom, _, FieldName},
                   [{typed_record_field,
                     {record_field, _, {atom, _, FieldName}, _}, Ty}|_], I) ->
    {I, Ty};
get_rec_field_index_and_type(FieldWithAnno, [_|RecFieldTypes], I) ->
    get_rec_field_index_and_type(FieldWithAnno, RecFieldTypes, I + 1);
get_rec_field_index_and_type(FieldWithAnno, [], _) ->
    throw(undef(record_field, FieldWithAnno)).

%% Helper for finding the return type of record_info/2
-spec get_record_info_type(erl_parse:abstract_expr(), env()) -> type().
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

record_field_types(Fields) ->
    lists:map(fun
                  (?type_field_type(Name, Type)) ->
                      {Name, Type};
                  (?typed_record_field(Name, Type)) ->
                      {Name, Type}
              end, Fields).

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
-spec type_check_form_with_timeout(expr(), [any()], boolean(), env(), [any()]) -> R when
      R :: badarg | any | [any()].
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
                     erlang:raise(throw, Crash, STrace);
                 {error_trace, Error, Trace} ->
                     ?verbose(Env, "Task reported error with trace from ~s~n",
                              [gradualizer_fmt:form_info(Function)]),
                     erlang:raise(error, Error, Trace);
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

-spec type_check_form(_, _, _, _, _) -> R when
      R :: {errors, list()}
         | {crash, any(), list()}
         | {error_trace, any(), list()}.
type_check_form(Function, Errors, StopOnFirstError, Env, Opts)
  when Errors =:= [];
       not StopOnFirstError ->
    CrashOnError = proplists:get_bool(crash_on_error, Opts),

    try type_check_function(Env, Function) of
        {_VarBinds, _Cs} ->
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
    Errors.

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
         infer = proplists:get_bool(infer, Opts),
         verbose = proplists:get_bool(verbose, Opts),
         union_size_limit = proplists:get_value(union_size_limit, Opts,
                                                default_union_size_limit())}.

default_union_size_limit() -> 30.

create_fenv(Specs, Funs) ->
% We're taking advantage of the fact that if a key occurs more than once
% in the list then it right-most occurrence will take precedence. In this
% case it will mean that if there is a spec, then that will take precedence
% over the default type any().
    maps:from_list(
      [ {{Name, NArgs}, type(any)}
        || {function,_, Name, NArgs, _Clauses} <- Funs
      ] ++
      [ {{Name, NArgs}, absform:normalize_function_type_list(Types)}
        || {{Name, NArgs}, Types} <- Specs
      ]
     ).

%% Collect the top level parse tree stuff returned by epp:parse_file/2.
-spec collect_specs_types_opaques_and_functions(Forms :: list()) -> #parsedata{}.
collect_specs_types_opaques_and_functions(Forms) ->
    aux(Forms, #parsedata{}).

%% Helper for collect_specs_types_opaques_and_functions/1
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
number_of_exported_functions(Forms) ->
    ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
    case ParseData#parsedata.export_all of
        true -> length(ParseData#parsedata.functions);
        false -> length(ParseData#parsedata.exports)
    end.

line_no(Expr) ->
    erl_anno:line(element(2, Expr)).

-spec type_error(type_error()) -> error().
type_error(Kind) ->
    {type_error, Kind}.

-spec type_error(type_error(), anno()) -> error().
type_error(Kind, P) ->
    {type_error, Kind, P}.

-spec type_error(expr(), type() | [type()], type()) -> error();
                (type_error(), anno(), type()) -> error().
type_error(Kind, P, Ty) ->
    {type_error, Kind, P, Ty}.


-spec type_error(cyclic_type_vars, anno(), bounded_function(), list()) -> error();
                (type_error(), anno(), atom() | pattern(), type()) -> error();
                (type_error(), unary_op(), anno(), type()) -> error().
type_error(Kind, P, Info, Ty) ->
    {type_error, Kind, P, Info, Ty}.

-spec type_error(call_arity, anno(), atom(), arity(), arity()) -> error();
                (type_error(), binary_op(), anno(), type(), type()) -> error().
type_error(Kind, Op, P, Ty1, Ty2) ->
    {type_error, Kind, Op, P, Ty1, Ty2}.

-spec undef(undef(), expr()) -> error().
undef(Kind, Info) ->
    {undef, Kind, Info}.

-spec undef(undef(), anno(), expr()) -> error().
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

-spec nonexhaustive(anno(), expr()) -> error().
nonexhaustive(P, Example) ->
    {nonexhaustive, P, Example}.

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
error_evidence({_, _, _, _, _, Evidence}) -> Evidence.
