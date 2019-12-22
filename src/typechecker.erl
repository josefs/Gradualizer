-module(typechecker).

%% API used by gradualizer.erl
-export([type_check_forms/2,
         print_errors/2
        ]).

%% Export all for easier testing and debugging while the project is
%% still in an early stage
-compile([export_all]).

-include("typelib.hrl").

-ifdef(OTP_RELEASE).
-compile([{nowarn_deprecated_function,{erlang,get_stacktrace,0}}]).
-endif.

%% Performance hack: Unions larger than this value are replaced by any() in normalization.
-define(union_size_limit, 20).

-define(verbose(Env, Fmt, Args),
        case Env#env.verbose of
            true -> io:format(Fmt, Args);
            false -> ok
        end).

-define(throw_orig_type(EXPR, ORIGTYPE, NORMTYPE),
        try EXPR
        catch
            throw:TypeError when element(size(TypeError), TypeError) =:= NORMTYPE ->
                %% if the last element of the type_error tuple is the normalized type
                %% replace it with the original result type
                ST = erlang:get_stacktrace(),
                erlang:raise(throw, setelement(size(TypeError), TypeError, ORIGTYPE), ST)
        end).

-export_type([typed_record_field/0]).

-type type() :: gradualizer_type:abstract_type().

%% Pattern macros
-define(type(T), {type, _, T, []}).
-define(type(T, A), {type, _, T, A}).

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

-type typed_record_field() :: {typed_record_field,
                               {record_field, erl_anno:anno(), Name :: {atom, erl_anno:anno(), atom()},
                                DefaultValue :: gradualizer_type:abstract_expr()},
                                Type :: type()}.

%% Type environment, passed around while comparing compatible subtypes
-record(tenv, {module :: module(),
               types = #{} :: #{{Name :: atom(), arity()} => {Params :: [atom()],
                                                              Body :: type()}},
               records = #{} :: #{Name :: atom()          => [typed_record_field()]}
              }).

%%% The environment passed around during typechecking.
-record(env, {fenv     = #{}
             ,imported = #{}   :: #{{atom(), arity()} => module()}
             ,venv     = #{}
             ,tenv             :: #tenv{}
             ,infer    = false :: boolean()
             ,verbose  = false :: boolean()
             ,exhaust  = true  :: boolean()
             %,tyvenv  = #{}
             }).

%% Two types are compatible if one is a subtype of the other, or both.
compatible(Ty1, Ty2, TEnv) ->
    case {subtype(Ty1, Ty2, TEnv), subtype(Ty2, Ty1, TEnv)} of
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

-spec subtype(type(), type(), #tenv{}) -> {true, any()} | false.
subtype(Ty1, Ty2, TEnv) ->
    try compat(Ty1, Ty2, sets:new(), TEnv) of
        {_Memoization, Constraints} ->
            {true, Constraints}
    catch
        nomatch ->
            false
    end.

%% Check if at least on of the types in a list is a subtype of a type.
%% Used when checking intersection types.
any_subtype([], _Ty, _TEnv) ->
    false;
any_subtype([Ty1|Tys], Ty, TEnv) ->
    case subtype(Ty1, Ty, TEnv) of
        R={true, _} ->
            R;
        false ->
            any_subtype(Tys, Ty, TEnv)
    end.


% This function throws an exception in case of a type error

%% The functions compat and compat_ty are mutually recursive.
%% The main entry point is compat and all recursive calls should go via compat.
%% The function compat_ty is just a convenience function to be able to
%% pattern match on types in a nice way.
compat(T1, T2, A, TEnv) ->
    Ty1 = typelib:remove_pos(normalize(T1, TEnv)),
    Ty2 = typelib:remove_pos(normalize(T2, TEnv)),
    case sets:is_element({Ty1, Ty2}, A) of
        true ->
            ret(A);
        false ->
            compat_ty(Ty1, Ty2, sets:add_element({Ty1, Ty2}, A), TEnv)
    end.

%% any() is used as the unknown type in the gradual type system
compat_ty({type, _, any, []}, _, A, _TEnv) ->
    ret(A);
compat_ty(_, {type, _, any ,[]}, A, _TEnv) ->
    ret(A);
%% Term is the top of the subtyping relation
compat_ty(_, {type, _, term, []}, A, _TEnv) ->
    ret(A);
%% None is the bottom of the subtyping relation
compat_ty({type, _, none, []}, _, A, _TEnv) ->
    ret(A);
%% Every type is subtype of itself
compat_ty(T, T, A, _TEnv) ->
    ret(A);

%% Variables
compat_ty({var, _, Var}, Ty, A, _TEnv) ->
    {A, constraints:upper(Var, Ty)};
compat_ty(Ty, {var, _, Var}, A, _TEnv) ->
    {A, constraints:lower(Var, Ty)};
compat_ty({ann_type, _, [{var, _, Var}, Ty1]}, Ty2, A, TEnv) ->
    {A1, Cs} = compat(Ty1, Ty2, A, TEnv),
    {A1, constraints:combine(Cs, constraints:upper(Var, Ty1))};
compat_ty(Ty1, {ann_type, _, [{var, _, Var}, Ty2]}, A, TEnv) ->
    {A1, Cs} = compat(Ty1, Ty2, A, TEnv),
    {A1, constraints:combine(Cs, constraints:upper(Var, Ty2))};



% TODO: There are several kinds of fun types.
% Add support for them all eventually
compat_ty({type, _, 'fun', [_, Res1]},
          {type, _, 'fun', [{type, _, any}, Res2]},
          A, TEnv) ->
    compat(Res1, Res2, A, TEnv);
compat_ty({type, _, 'fun', [{type, _, product, Args1}, Res1]},
          {type, _, 'fun', [{type, _, product, Args2}, Res2]},
          A, TEnv) ->
    {Ap, Cs} = compat_tys(Args2, Args1, A, TEnv),
    {Aps, Css} = compat(Res1, Res2, Ap, TEnv),
    {Aps, constraints:combine(Cs, Css)};

%% Unions
compat_ty({type, _, union, Tys1}, {type, _, union, Tys2}, A, TEnv) ->
    lists:foldl(fun (Ty1, {A1, C1}) ->
                    {A2, C2} = any_type(Ty1, Tys2, A1, TEnv),
                    {A2, constraints:combine(C1, C2)}
                end, {A, constraints:empty()}, Tys1);
compat_ty(Ty1, {type, _, union, Tys2}, A, TEnv) ->
    any_type(Ty1, Tys2, A, TEnv);
compat_ty({type, _, union, Tys1}, Ty2, A, TEnv) ->
    all_type(Tys1, Ty2, A, TEnv);

% Integer types
compat_ty(Ty1, Ty2, A, _TEnv) when ?is_int_type(Ty1), ?is_int_type(Ty2) ->
    R1 = int_type_to_range(Ty1),
    R2 = int_type_to_range(Ty2),
    case lower_bound_less_or_eq(R2, R1) andalso
        upper_bound_more_or_eq(R2, R1) of
        true -> ret(A);
        false -> throw(nomatch)
    end;

%% Atoms
compat_ty({atom, _, _Atom}, {type, _, atom, []}, A, _TEnv) ->
    ret(A);

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
          A, _TEnv)
  when N2 > 0, M1 >= M2,
       N1 rem N2 == 0,
       (M1 - M2) rem N2 == 0 ->
    ret(A);

%% Records with the same name, defined in differend modules
%% TODO: Record equivallend on tuple form
compat_ty({type, P1, record, [{atom, _, Name}]},
          {type, P2, record, [{atom, _, Name}]}, A, TEnv) ->
    Fields1 = get_maybe_remote_record_fields(Name, P1, TEnv),
    Fields2 = get_maybe_remote_record_fields(Name, P2, TEnv),
    compat_record_fields(Fields1, Fields2, A, TEnv);

compat_ty({type, _, record, _}, {type, _, tuple, any}, A, _TEnv) ->
    ret(A);

%% Lists
compat_ty(Ty1, Ty2, A, TEnv) when ?is_list_type(Ty1), ?is_list_type(Ty2) ->
    {Empty1, Elem1, Term1} = list_view(Ty1),
    {Empty2, Elem2, Term2} = list_view(Ty2),
    case {Empty1, Empty2} of
        {E, E}   -> ok;
        {_, any} -> ok;
        _        -> throw(nomatch)
    end,
    compat_tys([Elem1, Term1], [Elem2, Term2], A, TEnv);

%% Tuples
compat_ty({type, _, tuple, any}, {type, _, tuple, _Args}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, tuple, _Args}, {type, _, tuple, any}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, tuple, Args1}, {type, _, tuple, Args2}, A, TEnv) ->
    compat_tys(Args1, Args2, A, TEnv);
%compat_ty({user_type, _, Name, Args}, Ty, A, TEnv) ->
%    compat(unfold_user_type(Name, Args, TEnv), Ty, A, TEnv);

%% Maps
compat_ty({type, _, map, any}, {type, _, map, _Assocs}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, map, _Assocs}, {type, _, map, any}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, map, Assocs1}, {type, _, map, Assocs2}, A, TEnv) ->
    lists:foldl(fun (Assoc2, {As, Cs1}) ->
			begin
			    {Ax, Cs2} = any_type(Assoc2, Assocs1, As, TEnv),
			    {Ax, constraints:combine(Cs1, Cs2)}
			end
		end, ret(A), Assocs2);
compat_ty({type, _, AssocTag2, [Key2, Val2]},
          {type, _, AssocTag1, [Key1, Val1]}, A, TEnv)
        when AssocTag2 == map_field_assoc, AssocTag1 == map_field_assoc;
             AssocTag2 == map_field_exact, AssocTag1 == map_field_exact;
             AssocTag2 == map_field_assoc, AssocTag1 == map_field_exact ->
    %% For M1 <: M2, mandatory fields in M2 must be mandatory fields in M1
    {A1, Cs1} = compat(Key1, Key2, A, TEnv),
    {A2, Cs2} = compat(Val1, Val2, A1, TEnv),
    {A2, constraints:combine(Cs1, Cs2)};

compat_ty(_Ty1, _Ty2, _, _) ->
    throw(nomatch).


compat_tys([], [], A, _TEnv) ->
    ret(A);
compat_tys([Ty1|Tys1], [Ty2|Tys2], A, TEnv) ->
    {Ap, Cs} =
    compat(Ty1 ,Ty2, A, TEnv),
    {Aps, Css} = compat_tys(Tys1, Tys2, Ap, TEnv),
    {Aps, constraints:combine(Cs, Css)};
compat_tys(_Tys1, _Tys2, _, _) ->
    throw(nomatch).

%% Two records are compatible if they have the same name (defined in different
%% modules) and they have the same number of fields and the field types match.
compat_record_fields([], [], A, _TEnv) ->
    ret(A);
compat_record_fields([{typed_record_field, _NameAndDefaultValue1, T1} | Fs1],
                     [{typed_record_field, _NameAndDefaultValue2, T2} | Fs2],
                     A, TEnv) ->
    {A1, Cs1} = compat(T1, T2, A, TEnv),
    {A2, Cs2} = compat_record_fields(Fs1, Fs2, A1, TEnv),
    {A2, constraints:combine(Cs1, Cs2)};
compat_record_fields(_, _, _, _) ->
    %% Mismatching number of fields
    throw(nomatch).

%% Returns a successful matching of two types. Convenience function for when
%% there were no type variables involved.
ret(A) ->
    {A, constraints:empty()}.

any_type(_Ty, [], _A, _TEnv) ->
    throw(nomatch);
any_type(Ty, [Ty1|Tys], A, TEnv) ->
    try
        compat(Ty, Ty1, A, TEnv)
    catch
        nomatch ->
            any_type(Ty, Tys, A, TEnv)
    end.

%% @doc All types in `Tys' must be compatible with `Ty'.
%% Returns all the gather memoizations and constrains.
%% Does not return (throws `nomatch') if any of the types is not compatible.
all_type(Tys, Ty, A, TEnv) ->
    all_type(Tys, Ty, A, [], TEnv).

all_type([], _Ty, A, Css, _TEnv) ->
    {A, constraints:combine(Css)};
all_type([Ty1|Tys], Ty, AIn, Css, TEnv) ->
    {AOut, Cs} = compat_ty(Ty1, Ty, AIn, TEnv),
    all_type(Tys, Ty, AOut, [Cs|Css], TEnv).

get_maybe_remote_record_fields(RecName, Anno, TEnv) ->
    case typelib:get_module_from_annotation(Anno) of
        {ok, Module} ->
            %% A record type in another module, from an expanded remote type
            case gradualizer_db:get_record_type(Module, RecName) of
                {ok, TypedRecordFields} ->
                    TypedRecordFields;
                not_found ->
                    throw({undef, record, Anno, {Module, RecName}})
            end;
        none ->
            %% Local record type
            get_record_fields(RecName, Anno, TEnv)
    end.

get_record_fields(RecName, Anno, #tenv{records = REnv}) ->
    case REnv of
        #{RecName := Fields} ->
            Fields;
        _NotFound ->
            throw({undef, record, Anno, RecName})
    end.

%% Greatest lower bound
%% --------------------
%%
%% * Computes the maximal (in the subtyping hierarchy) type that is a subtype
%%   of two given types.

-spec glb(type(), type(), TEnv :: #tenv{}) -> {type(), constraints:constraints()}.
glb(T1, T2, TEnv) ->
    glb(T1, T2, #{}, TEnv).

-spec glb([type()], TEnv :: #tenv{}) -> {type(), constraints:constraints()}.
glb(Ts, TEnv) ->
    lists:foldl(fun (T, {TyAcc, Cs1}) ->
			{Ty, Cs2} = glb(T, TyAcc, TEnv),
			{Ty, constraints:combine(Cs1, Cs2)}
		end,
                {type(term), constraints:empty()},
                Ts).

glb(T1, T2, A, TEnv) ->
    case maps:is_key({T1, T2}, A) of
        %% If we hit a recursive case we approximate with none(). Conceivably
        %% you could do some fixed point iteration here, but let's wait for an
        %% actual use case.
        true -> {type(none), constraints:empty()};
        false ->
            case gradualizer_cache:get_glb(TEnv#tenv.module, T1, T2) of
                false ->
                    Ty1 = typelib:remove_pos(normalize(T1, TEnv)),
                    Ty2 = typelib:remove_pos(normalize(T2, TEnv)),
                    {Ty, Cs} = glb_ty(Ty1, Ty2, A#{ {Ty1, Ty2} => 0 }, TEnv),
                    NormTy = normalize(Ty, TEnv),
                    gradualizer_cache:store_glb(TEnv#tenv.module, T1, T2, {NormTy, Cs}),
                    {NormTy, Cs};
                TyCs ->
                    %% these two types have already been seen and calculated
                    TyCs
            end
    end.

%% If either type is any() we don't know anything
glb_ty({type, _, any, []} = Ty1, _Ty2, _A, _TEnv) ->
    ret(Ty1);
glb_ty(_Ty1, {type, _, any, []} = Ty2, _A, _TEnv) ->
    ret(Ty2);

%% term() is the top of the hierarchy
glb_ty({type, _, term, []}, Ty2, _A, _TEnv) ->
    ret(Ty2);
glb_ty(Ty1, {type, _, term, []}, _A, _TEnv) ->
    ret(Ty1);

%% none() is the bottom of the hierarchy
glb_ty({type, _, none, []} = Ty1, _Ty2, _A, _TEnv) ->
    ret(Ty1);
glb_ty(_Ty1, {type, _, none, []} = Ty2, _A, _TEnv) ->
    ret(Ty2);

%% glb is idempotent
glb_ty(Ty, Ty, _A, _TEnv) ->
    ret(Ty);

%% Type variables. TODO: can we get here with constrained type variables?
glb_ty({ann_type, _, [{var, _, _}, Ty1]}, Ty2, A, TEnv) ->
    glb(Ty1, Ty2, A, TEnv);
glb_ty(Ty1, {ann_type, _, [{var, _, _}, Ty2]}, A, TEnv) ->
    glb(Ty1, Ty2, A, TEnv);
glb_ty(Var = {var, _, _}, Ty2, _A, _TEnv) ->
    V = new_type_var(),
    {{var, erl_anno:new(0), V}
    ,constraints:add_var(V,
       constraints:combine(
	 constraints:upper(V, Var),
	 constraints:upper(V, Ty2)
	))};
glb_ty(Ty1, Var = {var, _, _}, _A, _TEnv) ->
    V = new_type_var(),
    {{var, erl_anno:new(0), V}
    ,constraints:add_var(V,
       constraints:combine(
	 constraints:upper(V, Var),
	 constraints:upper(V, Ty1)
	))};

%% Union types: glb distributes over unions
glb_ty({type, Ann, union, Ty1s}, Ty2, A, TEnv) ->
    {Tys, Css} = lists:unzip([ glb_ty(Ty1, Ty2, A, TEnv) || Ty1 <- Ty1s ]),
    {{type, Ann, union, Tys}, constraints:combine(Css)};
glb_ty(Ty1, {type, Ann, union, Ty2s}, A, TEnv) ->
    {Tys, Css} = lists:unzip([glb_ty(Ty1, Ty2, A, TEnv) || Ty2 <- Ty2s ]),
    {{type, Ann, union, Tys}, constraints:combine(Css)};

%% Atom types
glb_ty(Ty1 = {atom, _, _}, {type, _, atom, []}, _A, _TEnv) ->
    ret(Ty1);
glb_ty({type, _, atom, []}, Ty2 = {atom, _, _}, _A, _TEnv) ->
    ret(Ty2);

%% Number types
glb_ty(Ty1, Ty2, _A, _TEnv) when ?is_int_type(Ty1), ?is_int_type(Ty2) ->
    {Lo1, Hi1} = int_type_to_range(Ty1),
    {Lo2, Hi2} = int_type_to_range(Ty2),
    ret(type(union, int_range_to_types({int_max(Lo1, Lo2), int_min(Hi1, Hi2)})));

%% List types
glb_ty(Ty1, Ty2, A, TEnv) when ?is_list_type(Ty1), ?is_list_type(Ty2) ->
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
    {Elem, Cs1} = glb(Elem1, Elem2, A, TEnv),
    {Term, Cs2} = glb(Term1, Term2, A, TEnv),
    {from_list_view({Empty, Elem, Term}), constraints:combine(Cs1, Cs2)};

%% Tuple types
glb_ty(Ty1 = {type, _, tuple, Tys1}, Ty2 = {type, _, tuple, Tys2}, A, TEnv) ->
    case {Tys1, Tys2} of
        {any, _} -> ret(Ty2);
        {_, any} -> ret(Ty1);
        _ when length(Tys1) /= length(Tys2) -> ret(type(none));
        _ ->
	    {Tys, Css} =
		lists:unzip(lists:zipwith(fun(T1, T2) ->
						  glb(T1, T2, A, TEnv)
					  end,
					  Tys1, Tys2)),
            TupleType =
                case lists:any(fun(?type(none)) -> true; (_) -> false end,
                               Tys) of
                    true ->
                        type(none);
                    false ->
                        type(tuple, Tys)
                end,
	    {TupleType, constraints:combine(Css)}
    end;

%% Record types. Either exactly the same record (handled above) or tuple().
glb_ty(Ty1 = {type, _, record, _}, {type, _, tuple, any}, _A, _TEnv) ->
    ret(Ty1);
glb_ty({type, _, tuple, any}, Ty2 = {type, _, record, _}, _A, _TEnv) ->
    ret(Ty2);
glb_ty({type, _, record, _}, {type, _, record, _}, _A, _TEnv) ->
    ret(type(none));

%% Map types. These are a bit tricky and we can't reach this case in the
%% current code. For now going with a very crude approximation.
glb_ty(Ty1 = {type, _, map, Assocs1}, Ty2 = {type, _, map, Assocs2}, _A, _TEnv) ->
    case {Assocs1, Assocs2} of
        {any, _} -> ret(Ty2);
        {_, any} -> ret(Ty1);
        _        -> ret(type(none))
    end;

%% Binary types. For now approximate this by returning the smallest type if
%% they are comparable, otherwise none(). See the corresponding case in
%% compat_ty for the subtyping rule.
glb_ty(Ty1 = {type, _, binary, _},
       Ty2 = {type, _, binary, _}, _A, TEnv) ->
    case subtype(Ty1, Ty2, TEnv) of
        {true, _} -> ret(Ty1);    %% Will never produce constraints
        false ->
            case subtype(Ty2, Ty1, TEnv) of
                {true, _} -> ret(Ty2);
                false     -> ret(type(none))
            end
    end;

%% Function types. Would require lub on arguments for proper implementation.
%% For now pick biggest arguments when comparable and none() otherwise.
glb_ty({type, _, 'fun', [{type, _, product, Args1}, Res1]},
       {type, _, 'fun', [{type, _, product, Args2}, Res2]}, A, TEnv) ->
    NoConstraints = constraints:empty(),
    {Res, Cs} = glb(Res1, Res2, A, TEnv),
    Subtype =
        fun(Ts1, Ts2) ->
            try compat_tys(Ts1, Ts2, sets:new(), TEnv) of
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
       {type, _, 'fun', [{type, _, any}, Res2]}, A, TEnv) ->
    {Res, Cs} = glb(Res1, Res2, A, TEnv),
    {type('fun', [Any, Res]), Cs};

glb_ty({type, _, 'fun', [{type, _, any}, Res1]},
       {type, _, 'fun', [{type, _, product, _} = TArgs2, _]} = T2, A, TEnv) ->
    glb_ty(type('fun', [TArgs2, Res1]), T2, A, TEnv);
glb_ty({type, _, 'fun', [{type, _, product, _} = TArgs1, _]} = T1,
       {type, _, 'fun', [{type, _, any}, Res2]}, A, TEnv) ->
    glb_ty(T1, type('fun', [TArgs1, Res2]), A, TEnv);

%% normalize and remove_pos only does the top layer
glb_ty({type, _, Name, Args1}, {type, _, Name, Args2}, A, TEnv)
        when length(Args1) == length(Args2) ->
    {Args, Css} = lists:unzip([ glb(Arg1, Arg2, A, TEnv) || {Arg1, Arg2} <- lists:zip(Args1, Args2) ]),
    {type(Name, Args), constraints:combine(Css)};

%% Incompatible
glb_ty(_Ty1, _Ty2, _A, _TEnv) -> {type(none), constraints:empty()}.

%% Normalize
%% ---------
%%
%% * Expand user-defined and remote types on head level (except opaque types)
%% * Replace built-in type synonyms
%% * Flatten unions and merge overlapping types (e.g. ranges) in unions
-spec normalize(type(), TEnv :: #tenv{}) -> type().
normalize({type, _, union, Tys}, TEnv) ->
    Types = flatten_unions(Tys, TEnv),
    case merge_union_types(Types, TEnv) of
        []  -> type(none);
        [T] -> T;
        Ts when length(Ts) > ?union_size_limit -> type(any); % performance hack
        Ts  -> type(union, Ts)
    end;
normalize({user_type, P, Name, Args} = Type, TEnv) ->
    case typelib:get_module_from_annotation(P) of
        {ok, Module} ->
            %% Local type in another module, from an expanded remote type
            case gradualizer_db:get_type(Module, Name, Args) of
                {ok, T} ->
                    normalize(T, TEnv);
                opaque ->
                    Type;
                not_found ->
                    throw({undef, user_type, P, {Module, Name, length(Args)}})
            end;
        none ->
            %% Local user-defined type
            TypeId = {Name, length(Args)},
            case TEnv#tenv.types of
                #{TypeId := {Vars, Type0}} ->
                    VarMap = maps:from_list(lists:zip(Vars, Args)),
                    Type1 = typelib:substitute_type_vars(Type0, VarMap),
                    normalize(Type1, TEnv);
                _NotFound ->
                    throw({undef, user_type, P, {Name, length(Args)}})
            end
    end;
normalize({remote_type, P, [{atom, _, M}, {atom, _, N}, Args]}, TEnv) ->
    case gradualizer_db:get_exported_type(M, N, Args) of
        {ok, T} ->
            normalize(T, TEnv);
        opaque ->
            typelib:annotate_user_types(M, {user_type, P, N, Args});
        not_exported ->
            throw({not_exported, remote_type, P, {M, N, length(Args)}});
        not_found ->
            throw({undef, remote_type, P, {M, N, length(Args)}})
    end;
normalize({op, _, _, _Arg} = Op, _TEnv) ->
    erl_eval:partial_eval(Op);
normalize({op, _, _, _Arg1, _Arg2} = Op, _TEnv) ->
    erl_eval:partial_eval(Op);
normalize({type, Ann, range, [T1, T2]}, TEnv) ->
    {type, Ann, range, [normalize(T1, TEnv), normalize(T2, TEnv)]};
normalize(Type, _TEnv) ->
    expand_builtin_aliases(Type).

%% Replace built-in type aliases
-spec expand_builtin_aliases(type()) -> type().
expand_builtin_aliases({var, Ann, '_'}) ->
    {type, Ann, any, []};
expand_builtin_aliases({type, Ann, binary, []}) ->
    {type, Ann, binary, [{integer, Ann, 0}, {integer, Ann, 8}]};
expand_builtin_aliases({type, Ann, bitstring, []}) ->
    {type, Ann, binary, [{integer, Ann, 0}, {integer, Ann, 1}]};
expand_builtin_aliases({type, Ann, boolean, []}) ->
    {type, Ann, union, [{atom, Ann, true}, {atom, Ann, false}]};
expand_builtin_aliases({type, Ann, bool, []}) ->
    {type, Ann, union, [{atom, Ann, true}, {atom, Ann, false}]};
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
expand_builtin_aliases({type, Ann, function, []}) ->
    {type, Ann, 'fun', [{type, Ann, any}, {type, Ann , any,[]}]};
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
-spec flatten_unions([type()], #tenv{}) -> [type()].
flatten_unions(Tys, TEnv) ->
    [ FTy || Ty <- Tys, FTy <- flatten_type(normalize(Ty, TEnv), TEnv) ].

flatten_type({type, _, none, []}, _) ->
    [];
flatten_type({type, _, union, Tys}, TEnv) ->
    flatten_unions(Tys, TEnv);
flatten_type({ann_type, _, [_, Ty]}, TEnv) ->
    flatten_type(normalize(Ty, TEnv), TEnv);
flatten_type(Ty, _TEnv) ->
    [Ty].

%% Merges overlapping integer types (including ranges and singletons).
%% (TODO) Removes all types that are subtypes of other types in the same union.
%% Retuns a list of disjoint types.
merge_union_types(Types, _TEnv) ->
    case lists:any(fun ({type, _, term, []}) -> true; (_) -> false end,
                   Types) of
        true ->
            %% term() is among the types.
            [type(term)];
        false ->
            {IntegerTypes1, OtherTypes1} =
                lists:partition(fun is_int_type/1, Types),
            IntegerTypes2 = merge_int_types(IntegerTypes1),
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

-spec is_int_type(type()) -> boolean().
is_int_type({type, _, T, _})
  when T == pos_integer; T == non_neg_integer; T == neg_integer;
       T == integer; T == range -> true;
is_int_type({integer, _, _}) -> true;
is_int_type({char, _, _}) -> true;
is_int_type(_) -> false.

%% A type used while normalizing integer types. The ranges that are possible to
%% write in the type system, i.e. non_neg_integer(), pos_integer(),
%% neg_integer(), integer(), finite ranges, singletons and unions of these.
-type int_range() :: {neg_inf, -1 | non_neg_integer() | pos_inf} |
                     {neg_integer() | 0 | 1, pos_inf} |
                     {integer(), integer()}.

%% Merges integer types by sorting on the lower bound and then merging adjacent
%% ranges. Returns a list of mutually exclusive integer types.
%%
%% This is an adoption of the standard algorithm for merging intervals.
-spec merge_int_types([type()]) -> [type()].
merge_int_types([]) ->
    [];
merge_int_types(IntTypes) ->
    Ranges = lists:map(fun int_type_to_range/1, IntTypes),
    [T | Ts] = lists:sort(fun lower_bound_less_or_eq/2, Ranges),
    MergedRanges = merge_int_ranges_help(Ts, [T]),
    lists:append(lists:map(fun int_range_to_types/1, MergedRanges)).

merge_int_ranges_help([{R1, R2} = R | Rs], [{S1, S2} | StackTail] = Stack) ->
    NewStack = if
                   R1 == neg_inf; S2 == pos_inf; R1 =< S2 + 1 ->
                       %% Overlapping or adjacent ranges. Merge them.
                       [{S1, int_max(R2, S2)} | StackTail];
                   true ->
                       %% Not mergeable ranges. Push R to stack.
                       [R | Stack]
               end,
    merge_int_ranges_help(Rs, NewStack);
merge_int_ranges_help([], Stack) ->
    lists:reverse(Stack).

%% callback for sorting ranges.
-spec lower_bound_less_or_eq(int_range(), int_range()) -> boolean().
lower_bound_less_or_eq({A, _}, {B, _}) ->
    if
        A == neg_inf -> true;
        B == neg_inf -> false;
        true         -> A =< B
    end.

-spec upper_bound_more_or_eq(int_range(), int_range()) -> boolean().
upper_bound_more_or_eq({_, A}, {_, B}) ->
    if
        A == pos_inf -> true;
        B == pos_inf -> false;
        true         -> A >= B
    end.

int_min(A, B) when A == neg_inf; B == neg_inf   -> neg_inf;
int_min(pos_inf, B) -> B;
int_min(A, pos_inf) -> A;
int_min(A, B) when is_integer(A), is_integer(B) -> min(A, B).

int_max(A, B) when A == pos_inf; B == pos_inf   -> pos_inf;
int_max(neg_inf, B) -> B;
int_max(A, neg_inf) -> A;
int_max(A, B) when is_integer(A), is_integer(B) -> max(A, B).

int_negate(pos_inf) ->
    neg_inf;
int_negate(neg_inf) ->
    pos_inf;
int_negate(I) when is_integer(I) ->
    -I.

%% Integer type to range, where the bounds can be infinities in some cases.
-spec int_type_to_range(type()) -> int_range().
int_type_to_range({type, _, integer, []})              -> {neg_inf, pos_inf};
int_type_to_range({type, _, neg_integer, []})          -> {neg_inf, -1};
int_type_to_range({type, _, non_neg_integer, []})      -> {0, pos_inf};
int_type_to_range({type, _, pos_integer, []})          -> {1, pos_inf};
int_type_to_range({type, _, range, [{Tag1, _, I1}, {Tag2, _, I2}]})
  when Tag1 =:= integer orelse Tag1 =:= char,
       Tag2 =:= integer orelse Tag2 =:= char           -> {I1, I2};
int_type_to_range({char, _, I})                        -> {I, I};
int_type_to_range({integer, _, I})                     -> {I, I}.

%% Converts a range back to a type. Creates two types in some cases and zero
%% types if lower bound is greater than upper bound.
-spec int_range_to_types(int_range()) -> [type()].
int_range_to_types({neg_inf, pos_inf}) ->
    [type(integer)];
int_range_to_types({neg_inf, -1}) ->
    [type(neg_integer)];
int_range_to_types({neg_inf, 0}) ->
    [type(neg_integer), {integer, erl_anno:new(0), 0}];
int_range_to_types({neg_inf, I}) when I > 0 ->
    [type(neg_integer),
     {type, erl_anno:new(0), range, [{integer, erl_anno:new(0), 0}
                                    ,{integer, erl_anno:new(0), I}]}];
int_range_to_types({neg_inf, I}) when I < -1 ->
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), neg_inf}
                                    ,{integer, erl_anno:new(0), I}]}];
int_range_to_types({I, pos_inf}) when I < -1 ->
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), I}
                                    ,{integer, erl_anno:new(0), -1}]},
     type(non_neg_integer)];
int_range_to_types({-1, pos_inf}) ->
    [{integer, erl_anno:new(0), -1}, type(non_neg_integer)];
int_range_to_types({0, pos_inf}) ->
    [type(non_neg_integer)];
int_range_to_types({1, pos_inf}) ->
    [type(pos_integer)];
int_range_to_types({I, pos_inf}) when I > 1 ->
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), I}
                                    ,{integer, erl_anno:new(0), pos_inf}]}];
int_range_to_types({I, I}) ->
    [{integer, erl_anno:new(0), I}];
int_range_to_types({I, J}) when is_integer(I) andalso
				is_integer(J) andalso
				I < J ->
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), I}
                                    ,{integer, erl_anno:new(0), J}]}];
int_range_to_types({I, J}) when I > J ->
    [].

%% Input arg must be already normalized
negate_num_type({type, _, TyName, []} = Ty) when
      TyName =:= any;
      TyName =:= integer;
      TyName =:= float ->
    Ty;
negate_num_type({integer, P, I}) ->
    {integer, P, -I};
negate_num_type({type, P, union, Tys}) ->
    %% We normalize the result only to merge `0 | pos_integer()` =>
    %% `non_neg_integer()` and to have a nice increasing order of Tys.
    %% The incoming union type must be already normalized so it shouldn't
    %% contain any unresolved types. So it is ok to normalize the result with an
    %% empty TEnv.
    normalize({type, P, union, [negate_num_type(Ty)||Ty <- Tys]}, #tenv{});
negate_num_type(None = {type, _, none, []}) ->
    None;
negate_num_type(RangeTy) ->
    %% some kind of range type like `1..3' or `neg_integer()'
    {L, U} = int_type_to_range(RangeTy),
    L2 = int_negate(U),
    U2 = int_negate(L),
    case int_range_to_types({L2, U2}) of
        [Ty] ->
            Ty;
        Tys = [_, _] ->
            %% In some cases the result is two mutually exclusive type.
            %% (Currently only when `non_neg_integer()` => `neg_integer() | 0`)
            type(union, Tys)
    end.

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

-spec expect_list_type(type(), allow_nil_type | dont_allow_nil_type, #tenv{}) ->
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
expect_list_type(?type(term) = TermTy, _EmptyOrNot, _) ->
    {elem_ty, TermTy, constraints:empty()};
expect_list_type({type, _, maybe_improper_list, [ElemTy, _]}, _, _) ->
    {elem_ty, ElemTy, constraints:empty()};
expect_list_type({type, _, nil, []}, allow_nil_type, _) ->
    any;
expect_list_type({type, _, string, []}, _, _) ->
    {elem_ty, type(char), constraints:empty()};
expect_list_type({ann_type, _, [_, Ty]}, N, TEnv) ->
    expect_list_type(Ty, N, TEnv);
expect_list_type(Union = {type, _, union, UnionTys}, N, TEnv) ->
    {Tys, Cs} = expect_list_union(UnionTys, [], constraints:empty(), no_any, N, TEnv),
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
      constraints:upper(Var, {type, erl_anno:new(0), list, [TyVar]}))
    };
expect_list_type(Ty, _, _) ->
    {type_error, Ty}.


expect_list_union([Ty|Tys], AccTy, AccCs, Any, N, TEnv) ->
    case expect_list_type(normalize(Ty, TEnv), N, TEnv) of
        {type_error, _} ->
            expect_list_union(Tys, AccTy, AccCs, Any, N, TEnv);
        any ->
            expect_list_union(Tys
                             ,[type(any) | AccTy]
                             ,AccCs
                             ,any
                             ,N
                             ,TEnv);
        {elem_ty, NTy, Cs} ->
            expect_list_union(Tys
                             ,[NTy | AccTy]
                             ,constraints:combine(Cs, AccCs)
                             ,Any
                             ,N
                             ,TEnv);
        {elem_tys, NTys, Cs} ->
            expect_list_union(Tys
                             ,NTys ++ AccTy
                             ,constraints:combine(Cs, AccCs)
                             ,Any
                             ,N
                             ,TEnv)
    end;
expect_list_union([], AccTy, AccCs, any, _N, _TEnv) ->
    {[type(any) | AccTy], AccCs};
expect_list_union([], AccTy, AccCs, _NoAny, _N, _TEnv) ->
    {AccTy, AccCs}.

infer_literal_string("") ->
    type(nil);
infer_literal_string(Str) ->
    SortedChars = lists:usort(Str),
    if length(SortedChars) =< 10 ->
            %% heuristics: if there are not more than 10 different characters
            %% list them explicitely as singleton types
            CharTypes = [{char, erl_anno:new(0), C} || C <- SortedChars],
            type(nonempty_list, [normalize(type(union, CharTypes), #tenv{})]);
       true ->
            type(nonempty_list,
                 [type(range, [{char, erl_anno:new(0), hd(SortedChars)},
                               {char, erl_anno:new(0), lists:last(SortedChars)}])])
    end.

expect_tuple_type({type, _, tuple, any}, _N) ->
    any;
expect_tuple_type({type, _, tuple, Tys}, N) when length(Tys) == N ->
    {elem_ty, Tys, constraints:empty()};
expect_tuple_type(?type(term) = TermTy, N) ->
    {elem_ty, lists:duplicate(N, TermTy), constraints:empty()};
expect_tuple_type({ann_type, _, [_, Ty]}, N) ->
    expect_tuple_type(Ty, N);
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
-spec expect_fun_type(#env{}, type()) -> fun_ty() | {type_error, type()}.
expect_fun_type(Env, Type) ->
    case expect_fun_type1(Env, normalize(Type, Env#env.tenv)) of
        type_error -> {type_error, Type};
        Other -> Other
    end.

-spec expect_fun_type1(#env{}, type()) -> fun_ty() | type_error.
expect_fun_type1(Env, BTy = {type, _, bounded_fun, [Ft, _Fc]}) ->
    Sub = bounded_type_subst(Env#env.tenv, BTy),
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
expect_fun_type1(Env, {ann_type, _, [_, Ty]}) ->
    expect_fun_type1(Env, Ty);
expect_fun_type1(_Env, {var, _, Var}) ->
    ResTy = new_type_var(),
    {fun_ty_any_args, {var, erl_anno:new(0), ResTy}
    ,constraints:add_var(Var,
       constraints:upper(ResTy,
         {type, erl_anno:new(0), 'fun', [{type, erl_anno:new(0), any}
                                        ,{var,  erl_anno:new(0), ResTy}]}))};
expect_fun_type1(_Env, {type, _, any, []}) ->
    any;
expect_fun_type1(_Env, {type, _, term, []}) ->
    {fun_ty_any_args, type(term), constraints:empty()};
expect_fun_type1(_Env, _Ty) ->
    type_error.

-spec expect_intersection_type(#env{}, [tuple()]) -> [fun_ty()] | type_error.
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

-spec expect_fun_type_union(#env{}, [tuple()]) -> [fun_ty()].
expect_fun_type_union(_Env, []) ->
    [];
expect_fun_type_union(Env, [Ty|Tys]) ->
    case expect_fun_type1(Env, Ty) of
        type_error ->
            expect_fun_type_union(Env, Tys);
        TyOut ->
            [TyOut | expect_fun_type_union(Env, Tys)]
    end.

expect_record_type(Record, {type, _, record, [{atom, _, Name}]}, _TEnv) ->
    if Record == Name ->
         {ok, constraints:empty()};
       true ->
         type_error
    end;
expect_record_type(_Record, {type, _, tuple, any}, _TEnv) ->
    {ok, constraints:empty()};
expect_record_type(_Record, ?type(term), _TEnv) ->
    {ok, constraints:empty()};
expect_record_type(Record, {type, _, union, Tys}, TEnv) ->
    expect_record_union(Record, Tys, TEnv);
expect_record_type(Record, {var, _, Var}, _TEnv) ->
    {ok, constraints:add_var(Var,
           constraints:upper(Var, {record, erl_anno:new(0), Record}))};
expect_record_type(Record, {ann_type, _, [_, Ty]}, TEnv) ->
    expect_record_type(Record, Ty, TEnv);
expect_record_type(_, _, _) ->
    type_error.

expect_record_union(Record, [Ty | Tys], TEnv) ->
    case expect_record_type(Record, Ty, TEnv) of
      type_error ->
        expect_record_union(Record, Tys, TEnv);
      Res ->
        Res
    end;
expect_record_union(_Record, [], _TEnv) ->
    type_error.

new_type_var() ->
    I = erlang:unique_integer(),
    "_TyVar" ++ integer_to_list(I).

bounded_type_list_to_type(TEnv, Types) ->
    case unfold_bounded_type_list(TEnv, Types) of
        [Ty] -> Ty;
        Tys  -> type(union, Tys)
    end.

unfold_bounded_type_list(TEnv, Types) when is_list(Types) ->
    [ unfold_bounded_type(TEnv, Ty) || Ty <- Types ].

unfold_bounded_type(TEnv, BTy = {type, _, bounded_fun, [Ty, _]}) ->
    Sub = bounded_type_subst(TEnv, BTy),
    subst_ty(Sub, Ty);
unfold_bounded_type(_Env, Ty) -> Ty.

-spec bounded_type_subst(#tenv{}, {type, erl_anno:anno(), bounded_fun, [_]}) ->
        #{ atom() => type() }.
bounded_type_subst(TEnv, BTy = {type, P, bounded_fun, [_, Bounds]}) ->
    try
        solve_bounds(TEnv, Bounds)
    catch throw:{cyclic_dependencies, Xs} ->
        throw({type_error, cyclic_type_vars, P, BTy, Xs})
    end.

-spec solve_bounds(#tenv{}, [type()]) -> #{ atom() := type() }.
solve_bounds(TEnv, Cs) ->
    Defs = [ {X, T} || {type, _, constraint, [{atom, _, is_subtype}, [{var, _, X}, T]]} <- Cs ],
    Env  = lists:foldl(fun
                           ({_, ?type(term)}, E) ->
                               E; %% Don't unfold X :: term()
                           ({X, T}, E) ->
                               maps:update_with(X,
                                                fun(Ts) -> [T | Ts] end,
                                                [T],
                                                E)
                       end, #{}, Defs),
    DepG = maps:map(fun(_, T) -> maps:keys(free_vars(T)) end, Env),
    SCCs = gradualizer_lib:top_sort(DepG),
    solve_bounds(TEnv, Env, SCCs, #{}).

solve_bounds(TEnv, Defs, [{acyclic, X} | SCCs], Acc) ->
    %% TODO: Don't drop the constraints.
    {Ty1, _Cs} =
      case Defs of
	  #{X := Tys} ->
	      Tys1 = subst_ty(Acc, Tys),
	      %% Take intersection after substitution to
	      %% get rid of type variables.
	      lists:foldl(fun(S, {T, Css}) ->
				  {Ty, Cs} = glb(S, T, TEnv),
				  {Ty, constraints:combine(Cs, Css)}
			  end,
			  {type(term), constraints:empty()}, Tys1);
	  _NoBoundsForX ->
	      {type(any), constraints:empty()} %% or should we return term()?
      end,
    solve_bounds(TEnv, maps:remove(X, Defs), SCCs, Acc#{ X => Ty1 });
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
%-spec type_check_expr(#env, any()) -> { any(), #{ any() => any()}, #{ any() => any()} }.
type_check_expr(Env, Expr) ->
    Res = {Ty, _VarBinds, _Cs} = do_type_check_expr(Env, Expr),
    ?verbose(Env, "~sPropagated type of ~ts :: ~ts~n",
             [gradualizer_fmt:format_location(Expr, brief), erl_prettypr:format(Expr), typelib:pp_type(Ty)]),
    Res.

do_type_check_expr(Env, {var, P, Var}) ->
    case Env#env.venv of
        #{Var := Ty} ->
            return(Ty);
        #{} ->
            throw({unknown_variable, P, Var})
    end;
do_type_check_expr(Env, {match, _, Pat, Expr}) ->
    {Ty, VarBinds, Cs} = type_check_expr(Env, Expr),
    NormTy = normalize(Ty, Env#env.tenv),
    {_PatTy, UBoundNorm, Env2, Cs2} =
            ?throw_orig_type(add_type_pat(Pat, NormTy, Env#env.tenv, VarBinds),
                             Ty, NormTy),
    UBound = case UBoundNorm of NormTy -> Ty;
                                _Other -> UBoundNorm end,
    {UBound, Env2, constraints:combine(Cs,Cs2)};
do_type_check_expr(Env, {'if', _, Clauses}) ->
    infer_clauses(Env, Clauses);
do_type_check_expr(Env, {'case', _, Expr, Clauses}) ->
    {_ExprTy, VarBinds, Cs1} = type_check_expr(Env, Expr),
    VEnv = add_var_binds(Env#env.venv, VarBinds, Env#env.tenv),
    {Ty, VB, Cs2} = infer_clauses(Env#env{ venv = VEnv}, Clauses),
    {Ty, union_var_binds(VarBinds, VB, Env#env.tenv), constraints:combine(Cs1, Cs2)};
do_type_check_expr(Env, {tuple, P, TS}) ->
    { Tys, VarBindsList, Css} = lists:unzip3([ type_check_expr(Env, Expr)
                                              || Expr <- TS ]),
    InferredTy =
        case not Env#env.infer andalso
             lists:all(fun({type, _, any, []}) -> true;
                          (_) -> false
                       end, Tys) of
            true ->
                {type, P, any, []};
            false ->
                %% at least one element in the tuple has a type inferred from a spec
                {type, P, tuple, Tys}
        end,
    { InferredTy, union_var_binds(VarBindsList, Env#env.tenv), constraints:combine(Css) };
do_type_check_expr(Env, {cons, _, Head, Tail}) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Head),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Tail),
    VB = union_var_binds(VB1, VB2, Env#env.tenv),
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
                case expect_list_type(Ty2, dont_allow_nil_type, Env#env.tenv) of
                    any ->
                        {[type(any)], Cs};
                    {elem_ty, ElemTy, Cs3} ->
                        {[ElemTy], constraints:combine([Cs, Cs3])};
                    {elem_tys, ElemTys, Cs3} ->
                        {ElemTys, constraints:combine([Cs, Cs3])};
                    {type_error, ?type(nil)} ->
                        {[], Cs};
                    {type_error, BadTy} ->
                        throw({type_error, list, line_no(Tail), BadTy})
                        %% We throw a type error here because Tail is not of type list
                        %% (nor is it of type any()).
                        %% TODO: Improper list?
                end,
            FinalElemTy = normalize(type(union, [Ty1|TailElemTys]),
                                    Env#env.tenv),
            {type(nonempty_list, [FinalElemTy]), VB, Cs4}
    end;
do_type_check_expr(Env, {bin, _, BinElements} = BinExpr) ->
    %% <<Expr:Size/TypeSpecifierList, ...>>
    VarBindAndCsList =
        lists:map(fun ({bin_element, _P, Expr, _Size, _Specif} = BinElem) ->
                          %% Treat bin type specifier as type annotation
                          Ty = type_of_bin_element(BinElem),
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
     union_var_binds(VarBinds, Env#env.tenv),
     constraints:combine(Css)};
do_type_check_expr(Env, {call, _, {atom, _, TypeOp}, [Expr, {string, _, TypeStr} = TypeLit]})
  when TypeOp == '::'; TypeOp == ':::' ->
    %% Magic functions used as type annotation/assertion.
    try typelib:parse_type(TypeStr) of
        Type when TypeOp == '::' ->
            {VarBinds, Cs} = type_check_expr_in(Env, Type, Expr),
            {Type, VarBinds, Cs};
        Type when TypeOp == ':::' ->
            {InferredType, VarBinds, Cs1} = type_check_expr(Env, Expr),
            case compatible(InferredType, Type, Env#env.tenv) of
                {true, Cs2} ->
                   {Type, VarBinds, constraints:combine(Cs1, Cs2)};
                false ->
                    %% TODO: Error message improvement: Expected type
                    %% should be any type compatible with Type, rather
                    %% than just Type (the 3rd tuple element).
                    throw({type_error, Expr, Type, InferredType})
            end
    catch error:_ ->
        throw({bad_type_annotation, TypeLit})
    end;
do_type_check_expr(Env, {call, _, {atom, _, record_info}, [_, _]} = Call) ->
    Ty = get_record_info_type(Call, Env#env.tenv),
    {Ty, #{}, constraints:empty()};
do_type_check_expr(Env, {call, P, Name, Args}) ->
    {FunTy, VarBinds1, Cs1} = type_check_fun(Env, Name, length(Args)),
    {ResTy, VarBinds2, Cs2} = type_check_call_ty(Env, expect_fun_type(Env, FunTy), Args
                                                ,{Name, P, FunTy}),
    {ResTy, union_var_binds(VarBinds1, VarBinds2, Env#env.tenv),
            constraints:combine(Cs1, Cs2)};

do_type_check_expr(Env, {lc, _, Expr, Qualifiers}) ->
    type_check_comprehension(Env, lc, Expr, Qualifiers);
do_type_check_expr(Env, {bc, _, Expr, Qualifiers}) ->
    type_check_comprehension(Env, bc, Expr, Qualifiers);
do_type_check_expr(Env, {block, _, Block}) ->
    type_check_block(Env, Block);

% Don't return the type of anything other than something
% which ultimately comes from a function type spec.
do_type_check_expr(#env{infer = false}, {string, _, _}) ->
    return(type(any));
do_type_check_expr(#env{infer = false}, {nil, _}) ->
    return(type(any));
do_type_check_expr(#env{infer = false}, {atom, _, _Atom}) ->
    return(type(any));
do_type_check_expr(#env{infer = false}, {integer, _, _N}) ->
    return(type(any));
do_type_check_expr(#env{infer = false}, {float, _, _F}) ->
    return(type(any));
do_type_check_expr(#env{infer = false}, {char, _, _C}) ->
    return(type(any));

%% When infer = true, we do propagate the types of literals,
%% list cons, tuples, etc.
do_type_check_expr(#env{infer = true}, {string, _, Chars}) ->
    ActualyTy = infer_literal_string(Chars),
    return(ActualyTy);
do_type_check_expr(#env{infer = true}, {nil, _}) ->
    return(type(nil));
do_type_check_expr(#env{infer = true}, {atom, _, _} = Atom) ->
    return(Atom);
do_type_check_expr(#env{infer = true}, {integer, _, _N} = Integer) ->
    return(Integer);
do_type_check_expr(#env{infer = true}, {float, _, _F}) ->
    return(type(float));
do_type_check_expr(#env{infer = true}, {char, _, _C} = Char) ->
    return(Char);

%% Maps
do_type_check_expr(Env, {map, _, Assocs}) ->
    {_AssocTys, VB, Cs} = type_check_assocs(Env, Assocs),
    % TODO: When the --infer flag is set we should return the type of the map
    {type(any), VB, Cs};
do_type_check_expr(Env, {map, _, Expr, Assocs}) ->
    {Ty, VBExpr, Cs1} = type_check_expr(Env, Expr),
    {AssocTys, VBAssocs, Cs2} = type_check_assocs(Env, Assocs),
    MapTy = update_map_type(Ty, AssocTys),
    % TODO: Check the type of the map.
    {MapTy, union_var_binds(VBExpr, VBAssocs, Env#env.tenv), constraints:combine(Cs1, Cs2)};

%% Records
do_type_check_expr(Env, {record_field, Anno, Expr, Record, FieldWithAnno}) ->
    {VB, Cs} = type_check_expr_in(Env, {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]}, Expr),
    Rec = get_record_fields(Record, Anno, Env#env.tenv),
    Ty = get_rec_field_type(FieldWithAnno, Rec),
    {Ty, VB, Cs};
do_type_check_expr(Env, {record, Anno, Expr, Record, Fields}) ->
    RecTy = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    {VB1, Cs1} = type_check_expr_in(Env, RecTy, Expr),
    Rec = get_record_fields(Record, Anno, Env#env.tenv),
    {VB2, Cs2} = type_check_fields(Env, Rec, Fields),
    {RecTy, union_var_binds(VB1, VB2, Env#env.tenv), constraints:combine(Cs1, Cs2)};
do_type_check_expr(Env, {record, Anno, Record, Fields}) ->
    RecTy    = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    Rec      = get_record_fields(Record, Anno, Env#env.tenv),
    {VB, Cs} = type_check_fields(Env, Rec, Fields),
    {RecTy, VB, Cs};
do_type_check_expr(Env, {record_index, Anno, Name, FieldWithAnno}) ->
    case Env#env.infer of
        true ->
            RecFields = get_record_fields(Name, Anno, Env#env.tenv),
            Index = get_rec_field_index(FieldWithAnno, RecFields),
            return({integer, erl_anno:new(0), Index});
        false ->
            return(type(any))
    end;

%% Functions
do_type_check_expr(Env, {'fun', _, {clauses, Clauses}}) ->
    type_check_fun(Env, Clauses);
do_type_check_expr(Env, {'fun', P, {function, Name, Arity}}) ->
    case get_bounded_fun_type_list(Name, Arity, Env, P) of
        AnyType = {type, _, any, []} ->
            {AnyType, #{}, constraints:empty()};
        BoundedFunTypeList ->
            Ty = bounded_type_list_to_type(Env#env.tenv, BoundedFunTypeList),
            {Ty, #{}, constraints:empty()}
    end;
do_type_check_expr(Env, {'fun', P, {function, M, F, A}}) ->
    case {get_atom(Env, M), get_atom(Env, F), A} of
        {{atom, _, Module}, {atom, _, Function}, {integer, _, Arity}} ->
            case gradualizer_db:get_spec(Module, Function, Arity) of
                {ok, BoundedFunTypeList} ->
                    Ty = bounded_type_list_to_type(Env#env.tenv, BoundedFunTypeList),
                    {Ty, #{}, constraints:empty()};
                not_found ->
                    throw({call_undef, P, M, F, A})
            end;
        _ -> %% Not enough information to check the type of the call.
            {type(any), #{}, constraints:empty()}
    end;
do_type_check_expr(Env, {named_fun, _, FunName, Clauses}) ->
    %% Pick a type for the fun itself, to be used when checking references to
    %% itself inside the fun, e.g. recursive calls.
    FunTy = if
                Env#env.infer ->
                    %% Create a fun type of the correct arity
                    %% on the form fun((_,_,_) -> any()).
                    [{clause, _, Params, _Guards, _Block} | _] = Clauses,
                    Arity = length(Params),
                    create_fun_type(Arity, type(any));
                not Env#env.infer ->
                    type(any)
            end,
    NewEnv = Env#env{ venv = add_var_binds(#{FunName => FunTy}
                                          ,Env#env.venv, Env#env.tenv) },
    type_check_fun(NewEnv, Clauses);

do_type_check_expr(Env, {'receive', _, Clauses}) ->
    infer_clauses(Env, Clauses);
do_type_check_expr(Env, {'receive', _, Clauses, _After, Block}) ->
    {TyClauses, VarBinds1, Cs1} = infer_clauses(Env, Clauses),
    {TyBlock,   VarBinds2, Cs2} = type_check_block(Env, Block),
    {normalize({type, erl_anno:new(0), union, [TyClauses, TyBlock]}, Env#env.tenv)
    ,union_var_binds(VarBinds1, VarBinds2, Env#env.tenv)
    ,constraints:combine(Cs1, Cs2)};

%% Operators
do_type_check_expr(Env, {op, _, '!', Proc, Val}) ->
    % Message passing is always untyped, which is why we discard the types
    {_, VB1, Cs1} = type_check_expr(Env, Proc),
    {_, VB2, Cs2} = type_check_expr(Env, Val),
    {type(any)
    ,union_var_binds(VB1, VB2, Env#env.tenv)
    ,constraints:combine(Cs1, Cs2)};
do_type_check_expr(Env, {op, _, 'not', Arg} = Expr) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    BoolTy = type(boolean),
    case subtype(Ty, BoolTy, Env#env.tenv) of
        {true, Cs2} ->
            NormTy = normalize(Ty, Env#env.tenv),
            {negate_bool_type(NormTy), VB, constraints:combine(Cs1, Cs2)};
        false ->
            throw({type_error, Expr, Ty, BoolTy})
    end;
do_type_check_expr(Env, {op, _, 'bnot', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    IntTy = type(integer),
    case subtype(Ty, IntTy, Env#env.tenv) of
        {true, Cs2} ->
            {type(integer), VB, constraints:combine(Cs1, Cs2)};
        false ->
            throw({type_error, Arg, Ty, IntTy})
    end;
do_type_check_expr(Env, {op, P, '+', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    case subtype(Ty, {type, P, number ,[]}, Env#env.tenv) of
        {true, Cs2} ->
            {Ty, VB, constraints:combine(Cs1, Cs2)};
        false ->
            throw({type_error, non_number_argument_to_plus, P, Ty})
    end;
do_type_check_expr(Env, {op, P, '-', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    case subtype(Ty, {type, P, number ,[]}, Env#env.tenv) of
        {true, Cs2} ->
            NormTy = normalize(Ty, Env#env.tenv),
            {negate_num_type(NormTy), VB, constraints:combine(Cs1, Cs2)};
        false ->
            throw({type_error, non_number_argument_to_minus, P, Ty})
    end;
do_type_check_expr(Env, {op, P, BoolOp, Arg1, Arg2}) when
      (BoolOp == 'andalso') or (BoolOp == 'and') or
      (BoolOp == 'orelse')  or (BoolOp == 'or') or (BoolOp == 'xor') ->
    type_check_logic_op(Env, BoolOp, P, Arg1, Arg2);
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
    Env2 = Env#env{ venv = add_var_binds(VB, Env#env.venv, Env#env.tenv) },
    {TyC, _VB2, Cs2} = infer_clauses(Env2, CaseCs),
    {TyS, _VB3, Cs3} = infer_clauses(Env2, CatchCs),
    Cs4 = case AfterBlock of
              [] ->
                  constraints:empty();
              _ ->
                  {_TyA, _VB4, Cs5} = type_check_block(Env2, AfterBlock),
                  Cs5
          end,
    {normalize({type, erl_anno:new(0), union, [Ty, TyC, TyS]}, Env#env.tenv)
    ,VB
    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])}.

%% Helper for type_check_expr for funs
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
                    create_fun_type(Arity, RetTy)
            end,
    %% Variable bindings inside the fun clauses are local inside the fun.
    %% TODO: Solve constraints on the vars bound in each clause of the fun
    %% and propagate the rest of the constraints.
    {FunTy, #{}, constraints:empty()}.

%% Creates a type on the form fun((_,_,_) -> RetTy) with the given arity.
create_fun_type(Arity, RetTy) when is_integer(Arity) ->
    ParTys = lists:duplicate(Arity, type(any)),
    type('fun', [type(product, ParTys), RetTy]).

type_check_fields(Env, Rec, Fields) ->
    UnAssignedFields = get_unassigned_fields(Fields, Rec),
    type_check_fields(Env, Rec, Fields, UnAssignedFields).

type_check_fields(Env, Rec, [{record_field, _, {atom, _, _} = FieldWithAnno, Expr} | Fields]
                 ,UnAssignedFields) ->
    FieldTy = get_rec_field_type(FieldWithAnno, Rec),
    {VB1, Cs1} = type_check_expr_in(Env, FieldTy, Expr),
    {VB2, Cs2} = type_check_fields(Env, Rec, Fields, UnAssignedFields),
    {union_var_binds(VB1, VB2, Env#env.tenv), constraints:combine(Cs1,Cs2)};
type_check_fields(Env, Rec, [{record_field, _, {var, _, '_'}, Expr} | Fields]
                 ,UnAssignedFields) ->
    {VB1, Cs1} = type_check_fields(Env, Rec
                                  ,[ {record_field, erl_anno:new(0)
                                     ,{atom, erl_anno:new(0), Field}, Expr}
                                     || Field <- UnAssignedFields]
                                  ,should_not_be_inspected),
    {VB2, Cs2} = type_check_fields(Env, Rec, Fields, UnAssignedFields),
    {union_var_binds(VB1, VB2, Env#env.tenv), constraints:combine(Cs1,Cs2)};
type_check_fields(_Env, _Rec, [], _U) ->
    {#{}, constraints:empty()}.

get_unassigned_fields(Fields, All) ->
    [ Field || {typed_record_field,
                {record_field, _, {atom, _, Field}, _}, _} <- All] --
        [ Field || {record_field, _, {atom, _, Field}, _} <- Fields].

type_check_logic_op(Env, Op, P, Arg1, Arg2) ->
    % Bindings from the first argument are only passed along for
    % 'andalso' and 'orelse', not 'and' or 'or'.
    UnionVarBindsSecondArg =
        fun (VB1, VB2) ->
                if (Op == 'and') or (Op == 'or') or (Op == 'xor') ->
                        VB1;
                   (Op == 'andalso') or (Op == 'orelse') ->
                        union_var_binds(VB1, VB2, Env#env.tenv)
                end
        end,
    {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
    case subtype(Ty1, {type, P, bool, []}, Env#env.tenv) of
        false ->
            throw({type_error, Arg1, Ty1, type(boolean)});
        {true, Cs2} ->
            {Ty2, VB2, Cs3} = type_check_expr(Env#env{ venv = UnionVarBindsSecondArg(Env#env.venv,VB1 )}, Arg2),
            % Allow any() in second argument for shortcut operators
            SndArgTy = if Op == 'andalso'; Op == 'orelse' -> type(any);
                          true                            -> type(bool) end,
            case subtype(Ty2, SndArgTy, Env#env.tenv) of
                false ->
                    throw({type_error, Arg2, Ty2, type(boolean)});
                {true, Cs4} ->
                    Inferred =
                        case Op of
                            'andalso' -> type(union, [Ty1, {atom, erl_anno:new(0), false}]);
                            'orelse'  -> type(union, [Ty1, {atom, erl_anno:new(0), true}]);
                            _         -> type(boolean)
                        end,
                    {normalize(Inferred, Env#env.tenv)
                    ,union_var_binds(VB1, VB2, Env#env.tenv)
                    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])}
            end
    end.

type_check_rel_op(Env, Op, P, Arg1, Arg2) ->
    case {type_check_expr(Env, Arg1)
         ,type_check_expr(Env, Arg2)} of
        {{Ty1, VB1, Cs1}, {Ty2, VB2, Cs2}} ->
            case compatible(Ty1, Ty2, Env#env.tenv) of
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
                    ,union_var_binds(VB1, VB2, Env#env.tenv)
                    ,constraints:combine([Cs,Cs1,Cs2])};
                _ ->
                    throw({type_error, relop, Op, P, Ty1, Ty2})
            end
    end.

type_check_arith_op(Env, Op, P, Arg1, Arg2) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Arg2),

    case compat_arith_type(Ty1,Ty2) of
        false ->
          throw({type_error, arith_error, Op, P, Ty1, Ty2});
        {Ty, Cs3} ->
            {Ty
            ,union_var_binds(VB1, VB2, Env#env.tenv)
            ,constraints:combine([Cs1, Cs2, Cs3])}
    end.

type_check_int_op(Env, Op, P, Arg1, Arg2) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Arg2),

    case compat_arith_type(Ty1,Ty2) of
        false ->
            throw({type_error, int_error, Op, P, Ty1, Ty2});
        {{type, _, Ty, []}, _} when Ty == float orelse Ty == number ->
            throw({type_error, int_error, Op, P, Ty1, Ty2});
        {Ty, Cs3} ->
            {Ty
            ,union_var_binds(VB1, VB2, Env#env.tenv)
            ,constraints:combine([Cs1, Cs2, Cs3])}
    end.

type_check_list_op(Env, Arg1, Arg2) ->
  {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
  {Ty2, VB2, Cs2} = type_check_expr(Env, Arg2),

  ListTy = type(list),

  case {subtype(Ty1, ListTy, Env#env.tenv)
       ,subtype(Ty2, ListTy, Env#env.tenv)} of
    {{true, Cs3}, {true, Cs4}} ->
      {normalize({type, erl_anno:new(0), union, [Ty1, Ty2]}, Env#env.tenv)
      ,union_var_binds(VB1, VB2, Env#env.tenv)
      ,constraints:combine([Cs1, Cs2, Cs3, Cs4])
      };
    {false, _} ->
      throw({type_error, Arg1, Ty1, type(list)});
    {_, false} ->
        throw({type_error, Arg2, Ty2, type(list)})
  end.

type_check_call_ty(Env, {fun_ty, ArgsTy, ResTy, Cs}, Args, E) ->
    case {length(ArgsTy), length(Args)} of
        {L, L} ->
            {VarBindsList, Css} =
                lists:unzip(
                  [ type_check_expr_in(Env, ArgTy, Arg)
                    || {ArgTy, Arg} <- lists:zip(ArgsTy, Args)
                  ]),
            {ResTy
            ,union_var_binds(VarBindsList, Env#env.tenv)
            ,constraints:combine([Cs | Css])};
        {LenTy, LenArgs} ->
            P = element(2, E),
            throw({argument_length_mismatch, P, LenTy, LenArgs})
    end;
type_check_call_ty(Env, {fun_ty_any_args, ResTy, Cs}, Args, _E) ->
    {_Tys, VarBindsList, Css} =
        lists:unzip3(
          [ type_check_expr(Env, Arg)
            || Arg <- Args
          ]),
    {ResTy
    ,union_var_binds(VarBindsList, Env#env.tenv)
    ,constraints:combine([Cs | Css])};
type_check_call_ty(Env, any, Args, _E) ->
    {_Tys, VarBindsList, Css} =
        lists:unzip3(
          [ type_check_expr(Env, Arg)
            || Arg <- Args
          ]),
    {type(any)
    ,union_var_binds(VarBindsList, Env#env.tenv)
    ,constraints:combine(Css)};
type_check_call_ty(Env, {fun_ty_intersection, Tyss, Cs}, Args, E) ->
    {ResTy, VarBinds, CsI} = type_check_call_ty_intersect(Env, Tyss, Args, E),
    {ResTy, VarBinds, constraints:combine(Cs, CsI)};
type_check_call_ty(Env, {fun_ty_union, Tyss, Cs}, Args, E) ->
    {ResTy, VarBinds, CsI} = type_check_call_ty_union(Env, Tyss, Args, E),
    {ResTy, VarBinds, constraints:combine(Cs, CsI)};
type_check_call_ty(_Env, {type_error, _}, _Args, {Name, _P, FunTy}) ->
    throw({type_error, Name, FunTy, type('fun')}).

type_check_call_ty_intersect(_Env, [], _Args, {Name, P, FunTy}) ->
    throw({type_error, call_intersect, P, FunTy, Name});
type_check_call_ty_intersect(Env, [Ty | Tys], Args, E) ->
    try
        type_check_call_ty(Env, Ty, Args, E)
    catch
        Error when element(1,Error) == type_error ->
            type_check_call_ty_intersect(Env, Tys, Args, E)
    end.

type_check_call_ty_union(Env, Tys, Args, E) ->
    {ResTys, VBs, Css} =
        lists:unzip3([type_check_call_ty(Env, Ty, Args, E)
                      || Ty <- Tys]),
    {normalize(type(union, ResTys), Env#env.tenv),
     union_var_binds(VBs, Env#env.tenv),
     constraints:combine(Css)}.

compat_arith_type(Any = {type, _, any, []}, {type, _, any, []}) ->
    {Any, constraints:empty()};
compat_arith_type(Any = {type, _, any, []}, Ty) ->
    case subtype(Ty, type(number), #tenv{}) of
        false ->
            false;
        _ ->
            {Any, constraints:empty()}
    end;
compat_arith_type(Ty, Any = {type, _, any, []}) ->
    case subtype(Ty, type(number), #tenv{}) of
        false ->
            false;
        _ ->
            {Any, constraints:empty()}
    end;
compat_arith_type(Ty1, Ty2) ->
    TInteger = type(integer),
    case {subtype(Ty1, TInteger, #tenv{})
         ,subtype(Ty2, TInteger, #tenv{})} of
        {{true, C1}, {true, C2}} ->
            {TInteger, constraints:combine(C1, C2)};
        _ ->
            TFloat = type(float),
            case {subtype(Ty1, TFloat, #tenv{})
                 ,subtype(Ty2, TFloat, #tenv{})} of
                {{true, C1},{true, C2}} ->
                    {TFloat, constraints:combine(C1, C2)};
                _ ->
                    TNumber = type(number),
                    case {subtype(Ty1, TNumber, #tenv{})
                         ,subtype(Ty2, TNumber, #tenv{})} of
                        {{true, C1},{true, C2}} ->
                            {TNumber, constraints:combine(C1, C2)};
                        _ ->
                            false
                    end
            end
    end.

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
    {RetTy, #{}, Cs};
type_check_comprehension(Env, bc, Expr, []) ->
    {Ty, _VB, Cs} = type_check_expr(Env, Expr),
    RetTy = case normalize(Ty, Env#env.tenv) of
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
                    throw({type_error, Expr, NormTy, BitstringTy})
            end,
    {RetTy, #{}, Cs};
type_check_comprehension(Env, Compr, Expr, [{generate, _, Pat, Gen} | Quals]) ->
    {Ty,  _,  Cs1} = type_check_expr(Env, Gen),
    case expect_list_type(Ty, allow_nil_type, Env#env.tenv) of
        {elem_ty, ElemTy, Cs} ->
            {_PatTy, _UBound, NewVEnv, Cs2} =
                add_type_pat(Pat, ElemTy, Env#env.tenv, Env#env.venv),
            NewEnv = Env#env{venv = NewVEnv},
            {TyL, VB, Cs3} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
            {TyL, VB, constraints:combine([Cs, Cs1, Cs2, Cs3])};
        any ->
            NewVEnv = add_any_types_pat(Pat, Env#env.venv),
            NewEnv = Env#env{venv = NewVEnv},
            {TyL, VB, Cs2} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
            {TyL, VB, constraints:combine(Cs1, Cs2)};
        {elem_tys, _ElemTys, Cs} ->
            %% As a hack, we treat a union type as any, just to
            %% allow the program to type check.
            %% TODO: Rewrite the union outside of the comprehension
            NewVEnv = add_any_types_pat(Pat, Env#env.venv),
            NewEnv = Env#env{venv = NewVEnv},
            {TyL, VB, Cs2} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
            {TyL, VB, constraints:combine([Cs,Cs1,Cs2])};
        {type_error, BadTy} ->
            throw({type_error, Gen, BadTy, type(list)})
    end;
type_check_comprehension(Env, Compr, Expr, [{b_generate, _P, Pat, Gen} | Quals]) ->
    BitStringTy = type(binary, [{integer, erl_anno:new(0), 0},
                                {integer, erl_anno:new(0), 1}]),
    {VarBinds1, Cs1} =
        type_check_expr_in(Env, BitStringTy, Gen),
    {_PatTy, _UBound, NewVEnv, Cs2} =
        add_type_pat(Pat, BitStringTy, Env#env.tenv, Env#env.venv),
    {TyL, VarBinds2, Cs3} =
        type_check_comprehension(Env#env{venv = NewVEnv}, Compr, Expr, Quals),
    {TyL
    ,union_var_binds(VarBinds1, VarBinds2, Env#env.tenv)
    ,constraints:combine([Cs1, Cs2, Cs3])};
type_check_comprehension(Env, Compr, Expr, [Guard | Quals]) ->
    %% We don't require guards to return a boolean.
    %% This decision is up for debate.
    {_Ty, VarBinds1, Cs1} = type_check_expr(Env, Guard),
    NewEnv = Env#env{ venv = add_var_binds(Env#env.venv, VarBinds1, Env#env.tenv) },
    {TyL, VarBinds2, Cs2} = type_check_comprehension(NewEnv, Compr, Expr, Quals),
    {TyL, union_var_binds(VarBinds1, VarBinds2, Env#env.tenv), constraints:combine(Cs1, Cs2)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checking the type of an expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_check_expr_in(Env, ResTy, Expr) ->
    ?verbose(Env, "~sChecking that ~ts :: ~ts~n",
            [gradualizer_fmt:format_location(Expr, brief), erl_prettypr:format(Expr), typelib:pp_type(ResTy)]),
    NormResTy = normalize(ResTy, Env#env.tenv),
    ?throw_orig_type(do_type_check_expr_in(Env, NormResTy, Expr),
                     ResTy, NormResTy).

do_type_check_expr_in(Env, {type, _, any, []}, Expr) ->
    {_Ty, VB, Cs} = type_check_expr(Env, Expr),
    {VB, Cs};
do_type_check_expr_in(Env, Ty, {var, _, Name} = Var) ->
    VarTy = maps:get(Name, Env#env.venv),
    case subtype(VarTy, Ty, Env#env.tenv) of
        {true, Cs} ->
            {#{}, Cs};
        false ->
            throw({type_error, Var, VarTy, Ty})
    end;
do_type_check_expr_in(Env, Ty, {match, _, Pat, Expr}) ->
    {VarBinds, Cs} = type_check_expr_in(Env, Ty, Expr),
    {_PatTy, _UBound, NewVEnv, Cs2} =
        add_type_pat(Pat, Ty, Env#env.tenv, Env#env.venv),
    {union_var_binds(VarBinds, NewVEnv, Env#env.tenv), constraints:combine(Cs, Cs2)};
do_type_check_expr_in(Env, Ty, I = {integer, _, _}) ->
    case subtype(I, Ty, Env#env.tenv) of
        {true, Cs} ->
            {#{}, Cs};
        false ->
            throw({type_error, I, I, Ty})
    end;
do_type_check_expr_in(Env, Ty, {float, _, _} = Float) ->
    ExpectedType = type(float),
    case subtype(ExpectedType, Ty, Env#env.tenv) of
        {true, Cs} ->
            {#{}, Cs};
        false ->
            throw({type_error, Float, ExpectedType, Ty})
    end;
do_type_check_expr_in(Env, Ty, Atom = {atom, _, _}) ->
    case subtype(Atom, Ty, Env#env.tenv) of
        {true, Cs} ->
            {#{}, Cs};
        false ->
            throw({type_error, Atom, Atom, Ty})
    end;
do_type_check_expr_in(Env, Ty, Char = {char, _, _}) ->
    case subtype(Char, Ty, Env#env.tenv) of
        {true, Cs} ->
           {#{}, Cs};
       false ->
           throw({type_error, Char, Char, Ty})
    end;
do_type_check_expr_in(Env, Ty, Cons = {cons, _, H, T}) ->
    case expect_list_type(Ty, dont_allow_nil_type, Env#env.tenv) of
        {elem_ty, ETy, Cs} ->
            {VB1, Cs1} = type_check_expr_in(Env, ETy, H),
            {VB2, Cs2} = type_check_expr_in(Env, allow_empty_list(Ty),  T),
            {union_var_binds(VB1, VB2, Env#env.tenv), constraints:combine([Cs, Cs1, Cs2])};
        {elem_tys, ETys, Cs} ->
            {VB1, Cs1} = type_check_union_in(Env, ETys, H),
            {VB2, Cs2} = type_check_expr_in (Env, allow_empty_list(Ty), T),
            {union_var_binds(VB1, VB2, Env#env.tenv), constraints:combine([Cs, Cs1, Cs2])};
        any ->
            {_Ty, VB1, Cs1} = type_check_expr   (Env, H),
            {     VB2, Cs2} = type_check_expr_in(Env, allow_empty_list(Ty), T),
            {union_var_binds(VB1, VB2, Env#env.tenv), constraints:combine(Cs1, Cs2)};
        {type_error, _} ->
            throw({type_error, Cons, type(nonempty_list), Ty})
    end;
do_type_check_expr_in(Env, Ty, {nil, _} = Nil) ->
    case subtype(type(nil), Ty, Env#env.tenv) of
        {true, Cs} ->
            {#{}, Cs};
        false ->
            throw({type_error, Nil, type(nil), Ty})
    end;
do_type_check_expr_in(Env, Ty, {string, _, Chars} = String) ->
    ActualTy = infer_literal_string(Chars),
    case subtype(ActualTy, Ty, Env#env.tenv) of
      {true, Cs} ->
        {#{}, Cs};
      false ->
        throw({type_error, String, ActualTy, Ty})
    end;
do_type_check_expr_in(Env, Ty, {bin, _Anno, _BinElements} = Bin) ->
    BinTy = gradualizer_bin:compute_type(Bin),
    Cs1 = case subtype(BinTy, Ty, Env#env.tenv) of
              {true, Cs0} ->
                  Cs0;
              false ->
                  throw({type_error, Bin, BinTy, Ty})
          end,
    {_Ty, VarBinds, Cs2} = type_check_expr(Env, Bin),
    {VarBinds, constraints:combine(Cs1, Cs2)};
do_type_check_expr_in(Env, ResTy, {tuple, _, TS} = Tup) ->
    case expect_tuple_type(ResTy, length(TS)) of
        {elem_ty, Tys, Cs} ->
            {VBs, Css} = lists:unzip([ type_check_expr_in(Env, Ty, Expr)
                                    || {Ty, Expr} <- lists:zip(Tys, TS) ]),
            {union_var_binds(VBs, Env#env.tenv), constraints:combine([Cs|Css])};
        {elem_tys, Tyss, Cs} ->
            case type_check_tuple_union_in(Env, Tyss, TS) of
                none ->
                    {Ty, _VB, _Cs} = type_check_expr(Env#env{infer = true}, Tup),
                    throw({type_error, Tup, Ty, ResTy});
                {VBs, Css} ->
                    {union_var_binds(VBs, Env#env.tenv), constraints:combine([Cs|Css])}
            end;
        any ->
            {_Tys, VBs, Css} = lists:unzip3([type_check_expr(Env, Expr)
                                           || Expr <- TS ]),
            {union_var_binds(VBs, Env#env.tenv), constraints:combine(Css)};
        {type_error, _} ->
                    {Ty, _VB, _Cs} = type_check_expr(Env#env{infer = true}, Tup),
                    throw({type_error, Tup, Ty, ResTy})
    end;

%% Maps
do_type_check_expr_in(Env, ResTy, {map, _, Assocs} = MapCreation) ->
    {AssocTys, VBs, Cs2} = type_check_assocs(Env, Assocs),
    MapTy = update_map_type(type(map, []), AssocTys),
    case subtype(MapTy, ResTy, Env#env.tenv) of
        {true, Cs1} ->
            {VBs, constraints:combine(Cs1, Cs2)};
        false ->
            throw({type_error, MapCreation, MapTy, ResTy})
    end;
do_type_check_expr_in(Env, ResTy, {map, _, Expr, Assocs} = MapUpdate) ->
    {Ty, VBExpr, Cs1} = type_check_expr(Env, Expr),
    {AssocTys, VBAssocs, Cs2} = type_check_assocs(Env, Assocs),
    UpdatedTy = update_map_type(Ty, AssocTys),
    case subtype(UpdatedTy, ResTy, Env#env.tenv) of
        {true, Cs3} ->
            {union_var_binds(VBExpr, VBAssocs, Env#env.tenv),
             constraints:combine([Cs1, Cs2, Cs3])};
        false ->
            throw({type_error, MapUpdate, UpdatedTy, ResTy})
    end;

%% Records
do_type_check_expr_in(Env, ResTy, {record, Anno, Name, Fields} = Record) ->
    Rec = get_record_fields(Name, Anno, Env#env.tenv),
    case expect_record_type(Name, ResTy, Env#env.tenv) of
      type_error ->
            throw({type_error,
                   Record,
                   type(record, [{atom, erl_anno:new(0), Name}]),
                   ResTy});
      {ok, Cs1} ->
            {VarBinds, Cs2} = type_check_fields(Env, Rec, Fields),
            {VarBinds, constraints:combine(Cs1, Cs2)}
    end;
do_type_check_expr_in(Env, ResTy, {record, Anno, Exp, Name, Fields} = Record) ->
    RecordTy = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Name}]},
    case expect_record_type(Name, ResTy, Env#env.tenv) of
      type_error ->
        throw({type_error,
               Record,
               type(record, [{atom, erl_anno:new(0), Name}]),
               ResTy});
      {ok, Cs1} ->
            Rec = get_record_fields(Name, Anno, Env#env.tenv),
            {VarBindsList, Css}
                = lists:unzip(
                    lists:map(fun ({record_field, _, FieldWithAnno, Expr}) ->
                                      FieldTy = get_rec_field_type(FieldWithAnno, Rec),
                                      type_check_expr_in(Env, FieldTy, Expr)
                              end
                             ,Fields)
                   ),
            {VarBinds, Cs2} = type_check_expr_in(Env, RecordTy, Exp),
            {union_var_binds([VarBinds|VarBindsList], Env#env.tenv)
            ,constraints:combine([Cs1, Cs2|Css])}
    end;
do_type_check_expr_in(Env, ResTy, {record_field, Anno, Expr, Name, FieldWithAnno} = RecordField) ->
    Rec = get_record_fields(Name, Anno, Env#env.tenv),
    FieldTy = get_rec_field_type(FieldWithAnno, Rec),
    case subtype(FieldTy, ResTy, Env#env.tenv) of
        {true, Cs1} ->
            RecTy = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Name}]},
            {VarBinds, Cs2} = type_check_expr_in(Env, RecTy, Expr),
            {VarBinds, constraints:combine([Cs1,Cs2])};
        false ->
            throw({type_error, RecordField, FieldTy, ResTy})
    end;
do_type_check_expr_in(Env, ResTy, {record_index, Anno, Name, FieldWithAnno} = RecIndex) ->
    RecFields = get_record_fields(Name, Anno, Env#env.tenv),
    Index = get_rec_field_index(FieldWithAnno, RecFields),
    IndexTy = {integer, erl_anno:new(0), Index},
    case subtype(IndexTy, ResTy, Env#env.tenv) of
        {true, Cs} ->
            {#{}, Cs};
        false ->
            throw({type_error, RecIndex, IndexTy, ResTy})
    end;

do_type_check_expr_in(Env, ResTy, {'case', _, Expr, Clauses}) ->
    {ExprTy, VarBinds, Cs1} = type_check_expr(Env, Expr),
    Env2 = Env#env{ venv = add_var_binds(Env#env.venv, VarBinds, Env#env.tenv) },
    {VB, Cs2} = check_clauses(Env2, [ExprTy], ResTy, Clauses),
    {union_var_binds(VarBinds, VB, Env#env.tenv), constraints:combine(Cs1,Cs2)};
do_type_check_expr_in(Env, ResTy, {'if', _, Clauses}) ->
    check_clauses(Env, [], ResTy, Clauses);
do_type_check_expr_in(Env, ResTy,
                      {call, _, {atom, _, TypeOp},
                             [Expr, {string, _, TypeStr} = TypeLit]} = TyAnno)
  when TypeOp == '::'; TypeOp == ':::' ->
    try typelib:parse_type(TypeStr) of
        Type ->
            case subtype(Type, ResTy, Env#env.tenv) of
                {true, Cs1} when TypeOp == '::' ->
                    {VarBinds, Cs2} = type_check_expr_in(Env, Type, Expr),
                    {VarBinds, constraints:combine(Cs1, Cs2)};
                {true, Cs1} when TypeOp == ':::' ->
                    {InferredType, VarBinds, Cs2} = type_check_expr(Env, Expr),
                    case compatible(InferredType, Type, Env#env.tenv) of
                        {true, Cs3} ->
                            {VarBinds, constraints:combine([Cs1, Cs2, Cs3])};
                        false ->
                            %% TODO: Error message improvement: Expected type
                            %% should be any type compatible with Type, rather
                            %% than just Type (the 3rd tuple element).
                            throw({type_error, Expr, Type, InferredType})
                    end;
                false ->
                    throw({type_error, TyAnno, ResTy, Type})
            end
    catch error:_ ->
        throw({bad_type_annotation, TypeLit})
    end;
do_type_check_expr_in(Env, ResTy, {call, _, {atom, _, record_info}, [_, _]} = Call) ->
    Ty = get_record_info_type(Call, Env#env.tenv),
    case subtype(Ty, ResTy, Env#env.tenv) of
        {true, Cs} ->
            {#{}, Cs};
        false ->
            throw({type_error, Call, ResTy, Ty})
    end;
do_type_check_expr_in(Env, ResTy, {call, P, Name, Args} = OrigExpr) ->
    {FunTy, VarBinds, Cs} = type_check_fun(Env, Name, length(Args)),
    {VarBinds2, Cs2} = type_check_call(Env, ResTy, OrigExpr, expect_fun_type(Env, FunTy), Args,
                                       {P, Name, FunTy}),
    {union_var_binds(VarBinds, VarBinds2, Env#env.tenv), constraints:combine(Cs, Cs2)};
do_type_check_expr_in(Env, ResTy, {lc, P, Expr, Qualifiers} = OrigExpr) ->
    type_check_comprehension_in(Env, ResTy, OrigExpr, lc, Expr, P, Qualifiers);
do_type_check_expr_in(Env, ResTy, {bc, P, Expr, Qualifiers} = OrigExpr) ->
    type_check_comprehension_in(Env, ResTy, OrigExpr, bc, Expr, P, Qualifiers);

%% Functions
do_type_check_expr_in(Env, Ty, {'fun', _, {clauses, Clauses}} = Fun) ->
    case expect_fun_type(Env, Ty) of
        any ->
            {#{}, constraints:empty()};
        {fun_ty, ArgsTy, ResTy, Cs1} ->
            {VB, Cs2} = check_clauses(Env, ArgsTy, ResTy, Clauses),
            {VB, constraints:combine(Cs1, Cs2)};
        {fun_ty_any_args, ResTy, Cs1} ->
            {VB, Cs2} = check_clauses(Env, any, ResTy, Clauses),
            {VB, constraints:combine(Cs1, Cs2)};
        %% TODO: Can this case actually happen?
        {fun_ty_intersection, Tyss, Cs1} ->
            {VB, Cs2} = check_clauses_intersect(Env, Tyss, Clauses),
            {VB, constraints:combine(Cs1, Cs2)};
        {fun_ty_union, Tyss, Cs1} ->
            {VB, Cs2} = check_clauses_union(Env, Tyss, Clauses),
            {VB, constraints:combine(Cs1, Cs2)};
        {type_error, _} ->
            throw({type_error, Fun, type('fun'), Ty})
    end;
do_type_check_expr_in(Env, ResTy, Expr = {'fun', P, {function, Name, Arity}}) ->
    case get_bounded_fun_type_list(Name, Arity, Env, P) of
        ?type(any) when not Env#env.infer ->
            {#{}, constraints:empty()};
        ?type(any) ->
            FunType = create_fun_type(Arity, type(any)),
            case subtype(FunType, ResTy, Env#env.tenv) of
                {true, Cs} -> {#{}, Cs};
                false -> throw({type_error, Expr, FunType, ResTy})
            end;
        BoundedFunTypeList ->
            FunTypeList =
                unfold_bounded_type_list(Env#env.tenv, BoundedFunTypeList),
            case any_subtype(FunTypeList, ResTy, Env#env.tenv) of
                {true, Cs} -> {#{}, Cs};
                false -> throw({type_error, Expr, FunTypeList, ResTy})
            end
    end;
do_type_check_expr_in(Env, ResTy, Expr = {'fun', P, {function, M, F, A}}) ->
    case {get_atom(Env, M), get_atom(Env, F), A} of
        {{atom, _, Module}, {atom, _, Function}, {integer, _,Arity}} ->
            case gradualizer_db:get_spec(Module, Function, Arity) of
                {ok, BoundedFunTypeList} ->
                    FunTypeList =
                        unfold_bounded_type_list(Env#env.tenv, BoundedFunTypeList),
                    case any_subtype(FunTypeList, ResTy, Env#env.tenv) of
                        {true, Cs} -> {#{}, Cs};
                        false -> throw({type_error, Expr, FunTypeList, ResTy})
                    end;
                not_found ->
                    throw({call_undef, P, Module, Function, Arity})
            end;
        _ -> % We don't have enough information to check the type.
            {#{}, constraints:empty()}
    end;
do_type_check_expr_in(Env, Ty, {named_fun, _, FunName, Clauses} = Fun) ->
    NewEnv = Env#env{ venv = add_var_binds(#{ FunName => Ty }, Env#env.venv, Env#env.tenv) },
    case expect_fun_type(Env, Ty) of
        any ->
            {#{ FunName => Ty }, constraints:empty()};
        {fun_ty, ArgsTy, ResTy, Cs1} ->
            {VB, Cs2} = check_clauses(NewEnv, ArgsTy, ResTy, Clauses),
            {VB, constraints:combine(Cs1, Cs2)};
        {fun_ty_any_args, ResTy, Cs1} ->
            {VB, Cs2} = check_clauses(NewEnv, any, ResTy, Clauses),
            {VB, constraints:combine(Cs1, Cs2)};
        %% TODO: Can this case actually happen?
        {fun_ty_intersection, Tyss, Cs1} ->
            {VB, Cs2} = check_clauses_intersect(NewEnv, Tyss, Clauses),
            {VB, constraints:combine(Cs1, Cs2)};
        {fun_ty_union, Tyss, Cs1} ->
            {VB, Cs2} = check_clauses_union(Env, Tyss, Clauses),
            {VB, constraints:combine(Cs1, Cs2)};
        {type_error, _} ->
            throw({type_error, Fun, type('fun'), Ty})
    end;

do_type_check_expr_in(Env, ResTy, {'receive', _, Clauses}) ->
    check_clauses(Env, [type(any)], ResTy, Clauses);
do_type_check_expr_in(Env, ResTy, {'receive', _, Clauses, After, Block}) ->
    {VarBinds1, Cs1} = check_clauses(Env, [type(any)], ResTy, Clauses),
    {VarBinds2, Cs2} = type_check_expr_in(Env
                                         ,type(integer)
                                         ,After),
    {VarBinds3, Cs3} = type_check_block_in(Env, ResTy, Block),
    {union_var_binds([VarBinds1, VarBinds2, VarBinds3], Env#env.tenv)
                    ,constraints:combine([Cs1, Cs2, Cs3])};
do_type_check_expr_in(Env, ResTy, {op, _, '!', Arg1, Arg2}) ->
    % The first argument should be a pid.
    {_,  VarBinds1, Cs1} = type_check_expr(Env, Arg1),
    {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
    {union_var_binds(VarBinds1, VarBinds2, Env#env.tenv), constraints:combine(Cs1,Cs2)};
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
    % that we require ResTy to be any(), or perhaps also term().
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
                {_VB2, Cs2} = check_clauses(Env, [BlockTy], ResTy, CaseCs),
                constraints:combine(Cs1, Cs2)
        end,
    {_VB3, Cs3} = check_clauses(Env, [type(any)], ResTy, CatchCs),
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
    {#{}
    ,constraints:combine([Cs,Cs3,Cs5])}.

type_check_arith_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    type_check_arith_op_in(Env, number, ResTy, Op, P, Arg1, Arg2).

type_check_int_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    type_check_arith_op_in(Env, integer, ResTy, Op, P, Arg1, Arg2).

type_check_arith_op_in(Env, Kind, ResTy, Op, P, Arg1, Arg2) ->
    {ResTy1, Cs} = glb(type(Kind), ResTy, Env#env.tenv),
    case ResTy1 of
        %% TODO: allow none() if checking against none()? Not clear that
        %% this is sensible, since in that case you'd like to only require
        %% *one* of the arguments to be none(), not both.
        {type, _, none, []} ->
            Tag = if Kind == integer -> int_error;
                     true            -> arith_error end,
            throw({type_error, Tag, Op, P, ResTy});
        %% Fall back to inference mode if target is any()
        {type, _, any, []} ->
            {_, VB, Cs1} = type_check_arith_op(Env, Op, P, Arg1, Arg2),
            {VB, constraints:combine(Cs, Cs1)};
        _ ->
            case arith_op_arg_types(Op, ResTy1) of
                {ArgTy1, ArgTy2, Cs1} ->
                    {VarBinds1, Cs2} = type_check_expr_in(Env, ArgTy1, Arg1),
                    {VarBinds2, Cs3} = type_check_expr_in(Env, ArgTy2, Arg2),
                    {union_var_binds(VarBinds1, VarBinds2, Env#env.tenv),
                     constraints:combine([Cs, Cs1, Cs2, Cs3])};
                false ->
                    throw({type_error, op_type_too_precise, Op, P, ResTy1})
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

%% non_neg_integer() are closed under everything except '-' and '/'
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
    case int_type_to_range(Ty) of
        {0, B} when Op == 'rem' ->
            [TyR] = int_range_to_types({0, B + 1}),
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

type_check_logic_op_in(Env, ResTy, {op, _, Op, Arg1, Arg2} = OrigExpr) when Op == 'andalso'; Op == 'orelse' ->
    Bool   = type(boolean),
    Target = case Op of
                 'andalso' -> {atom, erl_anno:new(0), false};
                 'orelse'  -> {atom, erl_anno:new(0), true}
             end,
    case subtype(Target, ResTy, Env#env.tenv) of
        {true, Cs} ->
            {VarBinds1, Cs1} = type_check_expr_in(Env, Bool, Arg1),
            %% variable bindings are propagated from Arg1 to Arg2
            EnvArg2 = Env#env{venv = union_var_binds(Env#env.venv, VarBinds1, Env#env.tenv)},
            {VarBinds2, Cs2} = type_check_expr_in(EnvArg2, ResTy, Arg2),
            {union_var_binds(VarBinds1, VarBinds2, Env#env.tenv),
             constraints:combine([Cs, Cs1, Cs2])};
        false ->
            {Arg2Ty, _VB, _Cs} = type_check_expr(Env#env{infer = true}, Arg2),
            Ty = type(union, [Arg2Ty, Target]),
            throw({type_error, OrigExpr, Ty, ResTy})
    end;
type_check_logic_op_in(Env, ResTy, {op, _, _, Arg1, Arg2} = OrigExpr) ->
    Bool = type(boolean),
    case subtype(Bool, ResTy, Env#env.tenv) of
        {true, Cs} ->
          {VarBinds1, Cs1} = type_check_expr_in(Env, Bool, Arg1),
          {VarBinds2, Cs2} = type_check_expr_in(Env, Bool, Arg2),
          {union_var_binds(VarBinds1, VarBinds2, Env#env.tenv)
          ,constraints:combine([Cs, Cs1, Cs2])};
        false ->
          throw({type_error, OrigExpr, Bool, ResTy})
    end.

type_check_rel_op_in(Env, ResTy, {op, P, Op, Arg1, Arg2} = OrigExpr) ->
    Ty = type(boolean),
    case subtype(Ty, ResTy, Env#env.tenv) of
        {true, Cs0} ->
          {ResTy1, VarBinds1, Cs1} = type_check_expr(Env, Arg1),
          {ResTy2, VarBinds2, Cs2} = type_check_expr(Env, Arg2),
          case compatible(ResTy1, ResTy2, Env#env.tenv) of
              {true, Cs} ->
                  {union_var_binds(VarBinds1, VarBinds2, Env#env.tenv)
                  ,constraints:combine([Cs0, Cs1, Cs2, Cs])};
              false ->
                  throw({type_error, rel_error, Op, P, ResTy1, ResTy2})
          end;
        false ->
          throw({type_error, OrigExpr, Ty, ResTy})
    end.

type_check_list_op_in(Env, ResTy, {op, P, Op, Arg1, Arg2} = Expr) ->
    Target =
        %% '--' always produces a proper list, but '++' gives you an improper
        %% list if the second argument is improper.
        case Op of
            '--' -> type(list, [type(term)]);
            '++' -> type(maybe_improper_list, [type(term), type(term)])
        end,
    {ResTy1, Cs} = glb(Target, ResTy, Env#env.tenv),
    case ResTy1 of
        {type, _, none, []} ->
            % Tested by test/should_fail/list_op.erl subtract/1
            throw({type_error, Expr, type(list), ResTy});
        {type, _, any, []} ->
            {_, VB, Cs1} = type_check_list_op(Env, Arg1, Arg2),
            {VB, constraints:combine(Cs, Cs1)};
        _ ->
            case list_op_arg_types(Op, ResTy1) of
                {ArgTy1, ArgTy2} ->
                    {VarBinds1, Cs1} = type_check_expr_in(Env, ArgTy1, Arg1),
                    {VarBinds2, Cs2} = type_check_expr_in(Env, ArgTy2, Arg2),
                    {union_var_binds(VarBinds1, VarBinds2, Env#env.tenv),
                     constraints:combine([Cs, Cs1, Cs2])};
                false ->
                    throw({type_error, op_type_too_precise, Op, P, ResTy1})
            end
    end.

list_op_arg_types(ListOp, {type, _, union, Tys}) ->
    %% TODO: This approximates union of lists with list of unions
    Pairs = [list_op_arg_types(ListOp, Ty) || Ty <- Tys],
    case lists:member(false, Pairs) of
        true ->
            %% Some type in the union is not a list type
            false;
        false ->
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
    case list_view(Ty) of   %% Could go with [A] -- [term()] :: [A], but would miss legitimate errors
        false            -> false;
        {any, Elem, _}   -> {type(list, [Elem]), type(list, [Elem])};
        {empty, _, _}    -> {type(nil), type(list, [type(term)])};
        {nonempty, _, _} -> false
    end.

type_check_unary_op_in(Env, ResTy, Op, P, Arg) ->
    Target =
        case Op of
            'not'  -> type(boolean);
            'bnot' -> type(integer);
            '+'    -> type(number);
            '-'    -> type(number)
        end,
    {ResTy1, Cs} = glb(Target, ResTy, Env#env.tenv),
    case ResTy1 of
        %% TODO: allow if ResTy == none()?
        {type, _, none, []} ->
            throw({type_error, unary_error, Op, P, Target, ResTy});
        _ ->
            ArgTy = unary_op_arg_type(Op, ResTy1),
            {VB, Cs1} = type_check_expr_in(Env, ArgTy, Arg),
	    {VB, constraints:combine(Cs, Cs1)}
    end.

%% Which type should we check the argument against if we want the given type
%% out? We already know that Ty is a subtype of the return type of the operator.
unary_op_arg_type('+', Ty) -> Ty;
unary_op_arg_type(Op, {type, P, union, Tys}) ->
    {type, P, union, [ unary_op_arg_type(Op, Ty) || Ty <- Tys ]};
unary_op_arg_type('not', {atom, P, B}) -> {atom, P, not B}; %% boolean() = false | true
unary_op_arg_type(Op, Ty) when ?is_int_type(Ty), Op == '-' orelse Op == 'bnot' ->
    {Lo, Hi} = int_type_to_range(Ty),
    Neg = fun(pos_inf)             -> neg_inf;
             (neg_inf)             -> pos_inf;
             (N) when Op == '-'    -> -N;
             (N) when Op == 'bnot' -> bnot N end,
    type(union, int_range_to_types({Neg(Hi), Neg(Lo)}));
unary_op_arg_type('-', Ty = {type, _, float, []}) ->
    Ty.

%% Type check list comprehension or a binary comprehension
-spec type_check_comprehension_in(Env        :: #env{},
                                  ResTy      :: type(),
                                  OrigExpr   :: gradualizer_type:abstract_expr(),
                                  Compr      :: lc | bc,
                                  Expr       :: gradualizer_type:abstract_expr(),
                                  Position   :: erl_anno:anno(),
                                  Qualifiers :: [ListGen | BinGen | Filter]) ->
        {map(), constraints:constraints()}
       when
        ListGen :: {generate, erl_anno:anno(), gradualizer_type:abstract_expr(), gradualizer_type:abstract_expr()},
        BinGen  :: {b_generate, erl_anno:anno(), gradualizer_type:abstract_expr(), gradualizer_type:abstract_expr()},
        Filter  :: gradualizer_type:abstract_expr().
type_check_comprehension_in(Env, ResTy, OrigExpr, lc, Expr, _P, []) ->
    case expect_list_type(ResTy, allow_nil_type, Env#env.tenv) of
        any ->
            {_Ty, _VB, Cs} = type_check_expr(Env, Expr),
            {#{}, Cs};
        {elem_ty, ElemTy, Cs1} ->
            {_VB, Cs2} = type_check_expr_in(Env, ElemTy, Expr),
            {#{}, constraints:combine(Cs1, Cs2)};
        {elem_tys, ElemTys, Cs1} ->
            {VB, Cs2} = type_check_union_in(Env, ElemTys, Expr),
            {VB, constraints:combine(Cs1, Cs2)};
        {type_error, _} ->
            throw({type_error, OrigExpr, type(list), ResTy})
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
                     throw({type_error, OrigExpr, Ty, ResTy})
             end,
    {_VB, Cs} = type_check_expr_in(Env, ExprTy, Expr),
    {#{}, Cs};
type_check_comprehension_in(Env, ResTy, OrigExpr, Compr, Expr, P,
                            [{generate, _, Pat, Gen} | Quals]) ->
    {Ty, _VB1, Cs1} = type_check_expr(Env, Gen),
    case expect_list_type(Ty, allow_nil_type, Env#env.tenv) of
        any ->
            NewEnv = Env#env{venv = add_any_types_pat(Pat, Env#env.venv)},
            {_VB2, Cs2} = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
            {#{}, constraints:combine(Cs1, Cs2)};
        {elem_ty, ElemTy, Cs} ->
            {_PatTys, _UBounds, NewVEnv, Cs2} =
                add_types_pats([Pat], [ElemTy], Env#env.tenv, Env#env.venv),
            NewEnv = Env#env{venv = NewVEnv},
            {_VB2, Cs3} = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
            {#{}, constraints:combine([Cs, Cs1, Cs2, Cs3])};
        {elem_tys, _ElemTys, Cs} ->
            %% TODO: As a hack, we treat a union type as any, just to
            %% allow the program to type check.
            NewEnv = Env#env{venv = add_any_types_pat(Pat, Env#env.venv)},
            {_VB2, Cs2} = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
            {#{}, constraints:combine([Cs, Cs1, Cs2])};
        {type_error, BadTy} ->
            throw({type_error, Gen, BadTy, type(list)})
    end;
type_check_comprehension_in(Env, ResTy, OrigExpr, Compr, Expr, P,
                            [{b_generate, _P_Gen, Pat, Gen} | Quals]) ->
    %% Binary generator: Pat <= Gen
    %% Gen and Pat should be bitstrings (of any size).
    BitTy = {type, erl_anno:new(0), binary,
                   [{integer, erl_anno:new(0), 0},
                    {integer, erl_anno:new(0), 1}]},
    {_VarBindsGen, Cs1} = type_check_expr_in(Env, BitTy, Gen),
    {_PatTy, _UBound, NewVEnv, Cs2} =
        add_type_pat(Pat, BitTy, Env#env.tenv, Env#env.venv),
    NewEnv = Env#env{venv = NewVEnv},
    {VarBinds, Cs3} = type_check_comprehension_in(NewEnv, ResTy, OrigExpr, Compr, Expr, P, Quals),
    {VarBinds, constraints:combine([Cs1, Cs2, Cs3])};
type_check_comprehension_in(Env, ResTy, OrigExpr, Compr, Expr, P, [Pred | Quals]) ->
    %% We choose to check the type of the predicate here. Arguments can be
    %% made either way on whether we should check the type here.
    %% TODO: As a non-boolean predicates don't give runtime errors, we could
    %% possibly add a configuration parameter to toggle this check.
    {VB1, Cs1} = type_check_expr_in(Env, {type, erl_anno:new(0), 'boolean', []}, Pred),
    {VB2, Cs2} = type_check_comprehension_in(Env, ResTy, OrigExpr, Compr, Expr, P, Quals),
    {union_var_binds(VB1, VB2, Env#env.tenv), constraints:combine(Cs1, Cs2)}.

type_check_assocs(Env, [{Assoc, P, Key, Val}| Assocs])
  when Assoc == map_field_assoc orelse Assoc == map_field_exact ->
    {KeyTy, _KeyVB, Cs1} = type_check_expr(Env#env{infer = true}, Key),
    {ValTy, _ValVB, Cs2} = type_check_expr(Env#env{infer = true}, Val),
    {AssocTys, VB, Cs}   = type_check_assocs(Env, Assocs),
    {[{type, P, Assoc, [KeyTy, ValTy]} | AssocTys], VB
    ,constraints:combine([Cs, Cs1, Cs2])};
type_check_assocs(_Env, []) ->
    {[], #{}, constraints:empty()}.

update_map_type({ann_type, _, [_, Ty]}, AssocTys) ->
    %% FIXME we really should get rid of `ann_type's right in the
    %% begining or during normalization.
    update_map_type(Ty, AssocTys);
update_map_type({type, _, Ty, Arg}, AssocTys)
    when Ty == map, Arg == any;
	 Ty == any, Arg == []
	 ->
    type(map, [{type, P, map_field_exact, [Key, ValueType]}
	       || {type, P, _Assoc, [Key, ValueType]} <- AssocTys ]
         %% the original type could have any keys
         %% so we also need to include and optional any() => any()
         %% in the updated map type
         %% (map key types can be overlapping, precedence is left to right,
         %%  so let's append it as the last entry)
         ++ [type(map_field_assoc, [type(any), type(any)])]
        );
update_map_type({type, P, map, Assocs}, AssocTys) ->
    UpdatedAssocs = update_assocs(AssocTys,
                                  lists:map(fun typelib:remove_pos/1, Assocs)),
    {type, P, map, UpdatedAssocs}.

%% Override existing key's value types and append those key types
%% which are not updated
update_assocs([{type, P, _Assoc, [Key, ValueType]} | AssocTys],
	      Assocs) ->
    [{type, P, map_field_exact, [Key, ValueType]}|
     case take_assoc(typelib:remove_pos(Key), Assocs, []) of
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

type_check_fun(Env, {atom, P, Name}, Arity) ->
    % Local function call
    Types = get_bounded_fun_type_list(Name, Arity, Env, P),
    {Types, #{}, constraints:empty()};
type_check_fun(_Env, {remote, P, {atom,_,Module}, {atom,_,Fun}}, Arity) ->
    % Module:function call
    case gradualizer_db:get_spec(Module, Fun, Arity) of
        {ok, Types} ->
            Types1 = [ typelib:annotate_user_types(Module, T) || T <- Types ],
            {Types1, #{}, constraints:empty()};
        not_found   -> throw({call_undef, P, Module, Fun, Arity})
    end;
type_check_fun(_Env, {remote, _, _Expr, _}, Arity)->
    % Call to an unknown module. Revert to dynamic types.
    {[{type, erl_anno:new(0), bounded_fun,
       [{type, erl_anno:new(0), 'fun',
         [{type, erl_anno:new(0), product,
           lists:duplicate(Arity, type(any))},
          {type,0,any,[]}]},
        []]}], #{}, constraints:empty()};
type_check_fun(Env, Expr, _Arity) ->
    type_check_expr(Env, Expr).

type_check_call_intersection(Env, ResTy, OrigExpr, [Ty], Args, E) ->
    type_check_call(Env, ResTy, OrigExpr, Ty, Args, E);
type_check_call_intersection(Env, ResTy, OrigExpr, Tys, Args, E) ->
    type_check_call_intersection_(Env, ResTy, OrigExpr, Tys, Args, E).
type_check_call_intersection_(_Env, _ResTy, _, [], _Args, {P, Name, Ty}) ->
    throw({type_error, no_type_match_intersection, P, Name, Ty});
type_check_call_intersection_(Env, ResTy, OrigExpr, [Ty | Tys], Args, E) ->
    try
        type_check_call(Env, ResTy, OrigExpr, Ty, Args, E)
    catch
        Error when element(1, Error) == type_error ->
            type_check_call_intersection_(Env, ResTy, OrigExpr, Tys, Args, E)
    end.

type_check_call(_Env, _ResTy, _, {fun_ty, ArgsTy, _FunResTy, _Cs}, Args, {P, Name, _})
        when length(ArgsTy) /= length(Args) ->
    throw({type_error, call_arity, P, Name, length(ArgsTy), length(Args)});
type_check_call(Env, ResTy, OrigExpr, {fun_ty, ArgsTy, FunResTy, Cs}, Args, _) ->
    {VarBindsList, Css} =
        lists:unzip(
          lists:zipwith(fun (ArgTy, Arg) ->
                                type_check_expr_in(Env, ArgTy, Arg)
                        end, ArgsTy, Args)
         ),
    case subtype(FunResTy, ResTy, Env#env.tenv) of
        {true, Cs1} ->
            { union_var_binds(VarBindsList, Env#env.tenv)
            , constraints:combine([Cs, Cs1 | Css]) };
        false ->
            throw({type_error, OrigExpr, FunResTy, ResTy})
    end;
type_check_call(Env, ResTy, OrigExpr, {fun_ty_any_args, FunResTy, Cs}, Args, _)  ->
    {_Tys, VarBindsList, Css} =
        lists:unzip3(
          lists:map(fun (Arg) ->
                            type_check_expr(Env, Arg)
                    end, Args)
         ),
    case subtype(FunResTy, ResTy, Env#env.tenv) of
        {true, Cs1} ->
            { union_var_binds(VarBindsList, Env#env.tenv)
            , constraints:combine([Cs, Cs1 | Css]) };
        false ->
            throw({type_error, OrigExpr, FunResTy, ResTy})
    end;
type_check_call(Env, _ResTy, _, any, Args, _E) ->
    {_Tys, VarBindsList, Css} =
        lists:unzip3(
          lists:map(fun (Arg) ->
                            type_check_expr(Env, Arg)
                    end, Args)
         ),
    {union_var_binds(VarBindsList, Env#env.tenv), constraints:combine(Css)};
type_check_call(Env, ResTy, OrigExpr, {fun_ty_intersection, Tys, Cs1}, Args, E) ->
    {VB, Cs2} = type_check_call_intersection(Env, ResTy, OrigExpr, Tys, Args, E),
    {VB, constraints:combine(Cs1, Cs2)};
type_check_call(Env, ResTy, OrigExpr, {fun_ty_union, Tys, Cs1}, Args, E) ->
    {VB, Cs2} = type_check_call_union(Env, ResTy, OrigExpr, Tys, Args, E),
    {VB, constraints:combine(Cs1, Cs2)};
type_check_call(_Env, _ResTy, _, {type_error, _}, _Args, {_, Name, FunTy}) ->
    throw({type_error, Name, FunTy, type('fun')}).


type_check_call_union(_Env, _ResTy, _, [], _Args, _E) ->
    {#{}, constraints:empty()};
type_check_call_union(Env, ResTy, OrigExpr, [Ty|Tys], Args, E) ->
    {VB1, Cs1} = type_check_call(Env, ResTy, OrigExpr, Ty, Args, E),
    {VB2, Cs2} = type_check_call_union(Env, ResTy, OrigExpr, Tys, Args, E),
    %% TODO: It's not clear to me what should be returned here.
    %% When combining all the varbinds we should really create
    %% a union of all types for a variable.
    {union_var_binds(VB1, VB2, Env#env.tenv)
    ,constraints:combine(Cs1, Cs2)
    }.



type_check_block(Env, [Expr]) ->
    type_check_expr(Env, Expr);
type_check_block(Env, [Expr | Exprs]) ->
    {_, VarBinds, Cs1} = type_check_expr(Env, Expr),
    {Ty, VB, Cs2} = type_check_block(Env#env{ venv = add_var_binds(Env#env.venv, VarBinds, Env#env.tenv) }, Exprs),
    {Ty, add_var_binds(VB, VarBinds, Env#env.tenv), constraints:combine(Cs1, Cs2)}.

type_check_block_in(Env, ResTy, [Expr]) ->
    type_check_expr_in(Env, ResTy, Expr);
type_check_block_in(Env, ResTy, [Expr | Exprs]) ->
    {_, VarBinds, Cs1} = type_check_expr(Env, Expr),
    {VB, Cs2} = type_check_block_in(Env#env{ venv = add_var_binds(Env#env.venv, VarBinds, Env#env.tenv) }, ResTy, Exprs),
    {add_var_binds(VB, VarBinds, Env#env.tenv), constraints:combine(Cs1, Cs2)}.

type_check_union_in(Env, Tys, Expr) ->
    case type_check_union_in1(Env, Tys, Expr) of
        none        -> throw({type_error, mismatch, type(union, Tys), Expr});
        Ok = {_, _} -> Ok
    end.

type_check_union_in1(Env, [Ty|Tys], Expr) ->
    try
        type_check_expr_in(Env, Ty, Expr)
    catch
        E when element(1,E) == type_error ->
            type_check_union_in1(Env, Tys, Expr)
    end;
type_check_union_in1(_Env, [], _Expr) ->
    none.

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

type_check_cons_in(Env, Ty = {type, _, List, []}, H, T)
  when List == list orelse List == nonempty_list ->
    {_Ty, VB1, Cs1} = type_check_expr(Env, H),
    {     VB2, Cs2} = type_check_expr_in(Env, Ty, T),
    {union_var_binds(VB1, VB2, Env#env.tenv), constraints:combine(Cs1, Cs2)};
type_check_cons_in(Env, Ty = {type, _, List, [ElemTy]}, H, T)
    when List == list orelse List == nonempty_list ->
    {VB1, Cs1} = type_check_expr_in(Env, ElemTy, H),
    {VB2, Cs2} = type_check_expr_in(Env, Ty,     T),
    {union_var_binds(VB1, VB2, Env#env.tenv), constraints:combine(Cs1, Cs2)};
type_check_cons_in(Env, {type, _, union, Tys}, H, T) ->
    type_check_cons_union(Env, Tys, H, T);
type_check_cons_in(Env, {ann_type, _, [_, Ty]}, H, T) ->
    type_check_cons_in(Env, Ty, H, T).

type_check_cons_union(_Env, [], _H, _T) ->
    throw({type_error, cons_union});
type_check_cons_union(Env, [ Ty = {type, _, List, _} | Tys ], H, T)
    when List == list orelse List == nonempty_list ->
    try type_check_cons_in(Env, Ty, H, T)
    catch
        _ ->
            type_check_cons_union(Env, Tys, H, T)
    end;
type_check_cons_union(Env, [_ | Tys], H, T) ->
    type_check_cons_union(Env, Tys, H, T).


get_bounded_fun_type_list(Name, Arity, Env, P) ->
    case maps:find({Name, Arity}, Env#env.fenv) of
        {ok, Types} ->
            Types;
        error ->
            case erl_internal:bif(Name, Arity) of
                true ->
                    {ok, Types} = gradualizer_db:get_spec(erlang, Name, Arity),
                    Types;
                false ->
                    case get_imported_bounded_fun_type_list(Name, Arity, Env, P) of
                        {ok, Types} ->
                            Types;
                        error ->
                            throw({call_undef, P, Name, Arity})
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
                    throw({call_undef, P, Module, Name, Arity})
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
-spec infer_clauses(#env{}, [gradualizer_type:abstract_clause()]) ->
        {type(), VarBinds :: map(), constraints:constraints()}.
infer_clauses(Env, Clauses) ->
    {Tys, VarBindsList, Css} =
        lists:unzip3(lists:map(fun (Clause) ->
                                       infer_clause(Env, Clause)
                               end, Clauses)),
    {normalize(type(union, Tys), Env#env.tenv)
    ,union_var_binds(VarBindsList, Env#env.tenv)
    ,constraints:combine(Css)}.

-spec infer_clause(#env{}, gradualizer_type:abstract_clause()) ->
        {type(), VarBinds :: map(), constraints:constraints()}.
infer_clause(Env, {clause, _, Args, Guards, Block}) ->
    VEnv = add_any_types_pats(Args, Env#env.venv),
    EnvNew = Env#env{ venv = VEnv },
    % TODO: Can there be variable bindings in a guard? Right now we just
    % discard them.
    % TODO: Should we check that guards return boolean()?
    lists:map(fun (GuardConj) ->
                      lists:map(fun (Guard) ->
                                        type_check_expr(EnvNew, Guard)
                                end, GuardConj)
              end, Guards),
    {Ty, VB, Cs} = type_check_block(EnvNew, Block),
    {Ty, union_var_binds(VB, VEnv, Env#env.tenv), Cs}.


check_clauses_intersect(Env, Ty, Clauses) when not is_list(Ty) ->
    check_clauses_fun(Env, Ty, Clauses);
check_clauses_intersect(_Env, [], _Clauses) ->
    {#{}, constraints:empty()};
check_clauses_intersect(Env, [Ty|Tys], Clauses) ->
    {VarBinds1, Cs1} = check_clauses_fun(Env, Ty, Clauses),
    {VarBinds2, Cs2} = check_clauses_intersect(Env, Tys, Clauses),
    {union_var_binds(VarBinds1, VarBinds2, Env#env.tenv), constraints:combine(Cs1, Cs2)}.

check_clauses_union(_Env, [], _Clauses) ->
    %% TODO: Improve quality of type error
    throw({type_error, check_clauses});
check_clauses_union(Env, [Ty|Tys], Clauses) ->
    try
        check_clauses_fun(Env, Ty, Clauses)
    catch
        Error when element(1,Error) == type_error ->
            check_clauses_union(Env, Tys, Clauses)
    end.


check_clauses_fun(Env, {fun_ty, ArgsTy, FunResTy, Cs1}, Clauses) ->
    {VarBinds, Cs2} = check_clauses(Env, ArgsTy, FunResTy, Clauses),
    {VarBinds, constraints:combine(Cs1, Cs2)};
check_clauses_fun(Env, {fun_ty_any_args, FunResTy, Cs1}, Clauses) ->
    {VarBinds, Cs2} = check_clauses(Env, any, FunResTy, Clauses),
    {VarBinds, constraints:combine(Cs1, Cs2)};
check_clauses_fun(Env, any, Clauses) ->
    check_clauses(Env, any, type(any), Clauses);
check_clauses_fun(Env, {fun_ty_intersection, Tys, Cs1}, Clauses) ->
    {VarBinds, Cs2} = check_clauses_intersect(Env, Tys, Clauses),
    {VarBinds, constraints:combine(Cs1, Cs2)};
check_clauses_fun(Env, {fun_ty_union, Tys, Cs1}, Clauses) ->
    {VarBinds, Cs2} = check_clauses_union(Env, Tys, Clauses),
    {VarBinds, constraints:combine(Cs1, Cs2)}.

%% Checks a list of clauses (if/case/fun/try/catch/receive).
-spec check_clauses(Env :: #env{}, ArgsTy :: [type()] | any, ResTy :: type(),
                    Clauses :: [gradualizer_type:abstract_clause()]) ->
                        {VarBinds :: map(), constraints:constraints()}.
check_clauses(Env, any, ResTy, [{clause, _, Args, _, _} | _] = Clauses) ->
    %% 'any' is the ... in the type fun((...) -> ResTy)
    ArgsTy = lists:duplicate(length(Args), type(any)),
    check_clauses(Env, ArgsTy, ResTy, Clauses);
check_clauses(Env, [], {var, _, TyVar}, Clauses) ->
    %% This case was added for 'if' clauses.
    {Ty, VarBinds, Cs} = infer_clauses(Env, Clauses),
    {VarBinds, constraints:combine(constraints:upper(TyVar, Ty), Cs)};
check_clauses(Env, ArgsTy, ResTy, Clauses) ->
    {VarBindsList, Css, RefinedArgsTy} =
        lists:foldl(fun (Clause, {VBs, Css, RefinedArgsTy}) ->
                            {NewRefinedArgsTy, VB, Cs} =
                                check_clause(Env, RefinedArgsTy, ResTy, Clause),
                            {[VB | VBs], [Cs | Css], NewRefinedArgsTy}
                    end,
                    {[], [], ArgsTy},
                    Clauses),
    % Checking for exhaustive patternmatching
    case {Env#env.exhaust
         ,ArgsTy =/= any andalso
          lists:all(fun refinable/1, ArgsTy)
         ,lists:all(fun no_guards/1, Clauses)
         ,is_list(RefinedArgsTy) andalso
          lists:any(fun (T) -> T =/= type(none) end, RefinedArgsTy)} of
        {true, true, true, true} ->
            [{clause, P, _, _, _}|_] = Clauses,
            throw({nonexhaustive, P, gradualizer_lib:pick_value(RefinedArgsTy)});
        _ ->
            ok
    end,
    {union_var_binds(VarBindsList, Env#env.tenv), constraints:combine(Css)}.

%% This function checks clauses.
%% * If clauses have 0 arguments;
%% * case/try/catch/receive clauses have 1 argument;
%% * function clauses have any number of arguments;
%% * the patterns for catch C:E:T is represented as {C,E,T}
-spec check_clause(#env{}, [type()], type(), gradualizer_type:abstract_clause()) ->
        {RefinedTys :: [type()] , VarBinds :: map(), constraints:constraints()}.
check_clause(_Env, [?type(none)|_], _ResTy, {clause, P, _Args, _Guards, _Block}) ->
    throw({type_error, unreachable_clause, P});
check_clause(Env, ArgsTy, ResTy, C = {clause, P, Args, Guards, Block}) ->
    ?verbose(Env, "~sChecking clause :: ~s~n", [gradualizer_fmt:format_location(C, brief), typelib:pp_type(ResTy)]),
    case {length(ArgsTy), length(Args)} of
        {L, L} ->
            {PatTys, _UBounds, VEnv2, Cs1} =
                add_types_pats(Args, ArgsTy, Env#env.tenv, Env#env.venv),
            EnvNew      = Env#env{ venv =  VEnv2 },
            VarBinds1   = check_guards(EnvNew, Guards),
            EnvNewest   = EnvNew#env{ venv = add_var_binds(EnvNew#env.venv, VarBinds1, Env#env.tenv) },
            {VarBinds2, Cs2} = type_check_block_in(EnvNewest, ResTy, Block),
            RefinedTys  = refine_clause_arg_tys(ArgsTy, PatTys,
                                                Guards, Env#env.tenv),
            {RefinedTys
            ,union_var_binds([VarBinds1, VarBinds2, VEnv2], Env#env.tenv)
            ,constraints:combine(Cs1, Cs2)};
        {LenTy, LenArgs} ->
            throw({argument_length_mismatch, P, LenTy, LenArgs})
    end;
%% DEBUG
check_clause(_Env, _ArgsTy, _ResTy, Term) ->
    io:format("DEBUG: check_clause term: ~p~n", [Term]),
    throw(check_clause).

%% Refine types by matching clause. MatchedTys are the types exhausted by
%% each pattern in the previous clause.
refine_clause_arg_tys(Tys, MatchedTys, [], TEnv) ->
    Ty        = type(tuple, Tys),
    MatchedTy = type(tuple, MatchedTys),
    try refine(Ty, MatchedTy, TEnv) of
        ?type(tuple, RefTys) ->
            RefTys;
        ?type(none) ->
            lists:duplicate(length(Tys), type(none));
        ?type(union, _) ->
            Tys %% Multiple possibilities => don't refine
    catch
        no_refinement ->
            %% Imprecision prohibits refinement
            Tys;
        disjoint ->
            %% This can currently happen due to unhandled type variables, e.g.
            %% Elem :: T \ {attribute, _TyVar-54982374928, compile, export_all}
            %% No refinement.
            Tys
    end;
refine_clause_arg_tys(Tys, _TysBounds, _Guards, _TEnv) ->
    Tys.

%% Normalize, refine, revert normalize if no refinement.
%% May throw no_refinement.
refine(OrigTy, Ty, TEnv) ->
    NormTy = normalize(OrigTy, TEnv),
    case refine_ty(NormTy, normalize(Ty, TEnv), TEnv) of
        NormTy -> OrigTy;
        RefTy  -> RefTy
    end.

%% May throw no_refinement.
refine_ty(_Ty, ?type(none), _TEnv) ->
    %% PatTy none() means the pattern can't be used for refinement,
    %% because there is imprecision.
    throw(no_refinement);
refine_ty(?type(T, A), ?type(T, A), _) ->
    type(none);
refine_ty(?type(union, UnionTys), Ty, TEnv) ->
    RefTys = lists:foldr(fun (UnionTy, Acc) ->
                             try refine(UnionTy, Ty, TEnv) of
                                 RefTy -> [RefTy|Acc]
                             catch
                                 disjoint -> [UnionTy|Acc]
                             end
                          end,
                          [], UnionTys),
    normalize(type(union, RefTys), TEnv);
refine_ty(?type(tuple, Tys1), ?type(tuple, Tys2), TEnv)
  when length(Tys1) > 0, length(Tys1) == length(Tys2) ->
    %% Non-empty tuple
    RefTys = [refine(Ty1, Ty2, TEnv) || {Ty1, Ty2} <- lists:zip(Tys1, Tys2)],
    %% {a|b, a|b} \ {a,a} => {b, a|b}, {a|b, b}
    TuplesElems = pick_one_refinement_each(Tys1, RefTys),
    Tuples = [type(tuple, TupleElems) || TupleElems <- TuplesElems],
    normalize(type(union, Tuples), TEnv);
refine_ty({ann_type, _, [_, Ty1]}, Ty2, TEnv) ->
    refine_ty(Ty1, Ty2, TEnv);
refine_ty(Ty1, {ann_type, _, [_, Ty2]}, TEnv) ->
    refine_ty(Ty1, Ty2, TEnv);
refine_ty({atom, _, A}, {atom, _, A}, _) ->
    type(none);
refine_ty(?type(list, A), ?type(nil), _TEnv) ->
    type(nonempty_list, A);
refine_ty({Tag1, _, M}, {Tag2, _, N}, _TEnv)
    when Tag1 == integer orelse Tag1 == char,
         Tag2 == integer orelse Tag2 == char ->
    if M == N -> type(none);
       M /= N -> throw(disjoint)
    end;
refine_ty(Ty, {Tag, _, V}, TEnv) when ?is_int_type(Ty),
                                      Tag == integer orelse Tag == char ->
    case Ty of
        ?type(non_neg_integer) when V == 0 -> type(pos_integer);
        ?type(non_neg_integer) when V < 0  -> throw(disjoint);
        ?type(pos_integer)     when V =< 0 -> throw(disjoint);
	?type(pos_integer) ->
	    Tys = int_range_to_types({1, V - 1}) ++
		  int_range_to_types({V + 1, pos_inf}),
	    normalize(type(union, Tys), TEnv);
        ?type(neg_integer)     when V >= 0 -> throw(disjoint);
	?type(neg_integer) ->
	    Tys = int_range_to_types({neg_inf, V - 1}) ++
		  int_range_to_types({V + 1, -1}),
	    normalize(type(union, Tys), TEnv);
        ?type(integer)         when V == 0 ->
            type(union, [type(neg_integer), type(pos_integer)]);
        ?type(range, [{_TagLo, _, Lo}, {_TagHi, _, Hi}]) ->
            if
                Lo =< V, V =< Hi ->
                    %% It's in the range
                    Tys = int_range_to_types({Lo, V - 1}) ++
                          int_range_to_types({V + 1, Hi}),
                    normalize(type(union, Tys), TEnv);
                true ->
                    throw(disjoint)
            end;
        _OtherIntType ->
            %% Not possible to subtract from other integer types
            throw(no_refinement)
    end;
refine_ty(Ty1, Ty2, TEnv) ->
    case glb(Ty1, Ty2, TEnv) of
        {?type(none), _}  -> throw(disjoint);     %% disjoint
        _NotDisjoint -> throw(no_refinement) %% imprecision
    end.

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
refinable(?type(integer)) ->
    true;
refinable(?type(char)) ->
    true;
refinable(?type(non_neg_integer)) ->
    true;
refinable(?type(pos_integer)) ->
    true;
refinable(?type(neg_integer)) ->
    true;
refinable({atom, _, _}) ->
    true;
refinable(?type(nil)) ->
    true;
refinable(?type(union,Tys)) when is_list(Tys) ->
    lists:all(fun refinable/1, Tys);
refinable(?type(tuple, Tys)) when is_list(Tys) ->
    lists:all(fun refinable/1, Tys);
refinable({ann_type, _, [_, Ty]}) ->
    refinable(Ty);
refinable(_) ->
    false.

no_guards({clause, _, _, Guards, _}) ->
    Guards == [].

%% TODO: implement proper checking of guards.
check_guards(Env, Guards) ->
    union_var_binds(
      lists:map(fun (GuardSeq) ->
                        union_var_binds(
                          lists:map(fun (Guard) ->
                                                {_Ty, VB, _Cs} = type_check_expr(Env, Guard), % Do we need to thread the Env?
                                                VB
                                    end, GuardSeq), Env#env.tenv)
                end, Guards), Env#env.tenv).

type_check_function(Env, {function,_, Name, NArgs, Clauses}) ->
    ?verbose(Env, "Checking function ~p/~p~n", [Name, NArgs]),
    case maps:find({Name, NArgs}, Env#env.fenv) of
        {ok, FunTy} ->
            check_clauses_fun(Env, expect_fun_type(Env, FunTy), Clauses);
        error ->
            throw({internal_error, missing_type_spec, Name, NArgs})
    end.

-spec add_types_pats(Pats :: [gradualizer_type:abstract_pattern()],
                     Tys  :: [type()],
                     TEnv :: #tenv{},
                     VEnv :: map()) -> {PatTys      :: [type()],
                                        UBounds     :: [type()],
                                        NewVEnv     :: map(),
                                        Constraints :: constraints:constraints()}.
%% Type check patterns against types (P1 :: T1, P2 :: T2, ...)
%% and add variable bindings for the patterns.
%% Used for the arguments in clauses and the elements of tuples.
%%
%% The returned lists of types are interpreted like this:
%% PatTy :: Pat as if Pat were a type. For match-all patterns, PatTy
%% is the same as the type. For patterns matching a singleton type, PatTy
%% is the singleton type. Otherwise, PatTy is none(). PatTy is a type exhausted
%% by Pat. UBound is Ty or a subtype such that Pat :: UBound.
add_types_pats(Pats, Tys, TEnv, VEnv) ->
    add_types_pats(Pats, Tys, TEnv, VEnv, [], [], []).

add_types_pats([], [], _TEnv, VEnv, PatTysAcc, UBoundsAcc, CsAcc) ->
    {lists:reverse(PatTysAcc), lists:reverse(UBoundsAcc),
     VEnv, constraints:combine(CsAcc)};
add_types_pats([Pat | Pats], [Ty | Tys], TEnv, VEnv, PatTysAcc, UBoundsAcc, CsAcc) ->
    NormTy = normalize(Ty, TEnv),
    {PatTyNorm, UBoundNorm, VEnv2, Cs1} =
        ?throw_orig_type(add_type_pat(Pat, NormTy, TEnv, VEnv),
                         Ty, NormTy),
    %% De-normalize the returned types if they are the type checked against.
    PatTy  = case PatTyNorm  of NormTy -> Ty;
                                _      -> PatTyNorm end,
    UBound = case UBoundNorm of NormTy -> Ty;
                                _      -> UBoundNorm end,
    add_types_pats(Pats, Tys, TEnv, VEnv2,
                   [PatTy|PatTysAcc], [UBound|UBoundsAcc], [Cs1|CsAcc]).

%% Type check a pattern against a normalized type and add variable bindings.
-spec add_type_pat(Pat  :: gradualizer_type:abstract_pattern(),
                   Type :: type(),
                   TEnv :: #tenv{},
                   VEnv :: map()) ->
          {PatTy :: type(), UBound :: type(), NewVEnv :: map(),
           constraints:constraints()}.
add_type_pat({var, _, '_'}, Ty, _TEnv, VEnv) ->
    {Ty, Ty, VEnv, constraints:empty()};
add_type_pat({var, _, A} = Var, Ty, TEnv, VEnv) ->
    %% TODO: In a fun clause, A is always free, but not in e.g. case clauses
    case VEnv of
        #{A := VarTy} ->
            case subtype(VarTy, Ty, TEnv) of
                {true, Cs} ->
                    {type(none), VarTy, VEnv, Cs};
                false ->
                    throw({type_error, Var, VarTy, Ty})
            end;
        _FreeVar ->
            {Ty, Ty, VEnv#{A => Ty}, constraints:empty()}
    end;
add_type_pat(Expr, {type, _, any, []} = Ty, _TEnv, VEnv) ->
    {type(none), Ty, add_any_types_pat(Expr, VEnv), constraints:empty()};
add_type_pat(Pat, ?type(union, UnionTys)=UnionTy, TEnv, VEnv) ->
    {PatTys, UBounds, VEnvs, Css} =
        lists:foldr(fun (Ty, {PatTysAcc, UBoundsAcc, VEnvAcc, CsAcc}=Acc) ->
                        try add_type_pat(Pat, Ty, TEnv, VEnv) of
                            {PatTy, UBound, NewVEnv, Cs} ->
                                {[PatTy|PatTysAcc],
                                 [UBound|UBoundsAcc],
                                 [NewVEnv|VEnvAcc],
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
            throw({type_error, pattern, Anno, Pat, UnionTy});
        _SomeTysMatched ->
            %% TODO: The constraints should be merged with *or* semantics
            %%       and var binds with intersection
	    {Ty, Cs} = glb(PatTys, TEnv),
            {Ty,
             normalize(type(union, UBounds), TEnv),
             union_var_binds(VEnvs, TEnv),
             constraints:combine([Cs|Css])}
    end;
add_type_pat(Lit = {integer, P, _}, Ty, TEnv, VEnv) ->
    case subtype(Lit, Ty, TEnv) of
        {true, Cs} ->
            {Lit, Lit, VEnv, Cs};
        false ->
            throw({type_error, pattern, P, Lit, Ty})
    end;
add_type_pat(Lit = {char, P, Val}, Ty, TEnv, VEnv) ->
    case subtype({integer, P, Val}, Ty, TEnv) of
        {true, Cs} ->
            {Lit, Lit, VEnv, Cs};
        false ->
            throw({type_error, pattern, P, Lit, Ty})
    end;
add_type_pat(Lit = {float, P, _}, Ty, TEnv, VEnv) ->
    case subtype(type(float), Ty, TEnv) of
        {true, Cs} ->
            {type(none), type(float), VEnv, Cs};
        false ->
            throw({type_error, pattern, P, Lit, Ty})
    end;
add_type_pat(Tuple = {tuple, P, Pats}, Ty, TEnv, VEnv) ->
    case expect_tuple_type(Ty, length(Pats)) of
        any ->
            {type(none)
            ,Ty
            ,union_var_binds([add_any_types_pat(Pat, VEnv) || Pat <- Pats], TEnv)
            ,constraints:empty()};
        {elem_ty, Tys, Cs} ->
            {PatTys, UBounds, VEnv1, Cs1} = add_types_pats(Pats, Tys, TEnv, VEnv),
            {type(tuple, PatTys)
            ,type(tuple, UBounds)
            ,VEnv1
            ,constraints:combine(Cs, Cs1)};
        {type_error, _Type} ->
            throw({type_error, pattern, P, Tuple, Ty})
    end;
add_type_pat(Atom = {atom, P, _}, Ty, TEnv, VEnv) ->
    case subtype(Atom, Ty, TEnv) of
        {true, Cs} ->
            {Atom, Atom, VEnv, Cs};
        false ->
            throw({type_error, pattern, P, Atom, Ty})
    end;
add_type_pat(Nil = {nil, P}, Ty, TEnv, VEnv) ->
    case subtype(NilTy = type(nil), Ty, TEnv) of
        {true, Cs} ->
            {NilTy, NilTy, VEnv, Cs};
        false ->
            throw({type_error, pattern, P, Nil, Ty})
    end;
add_type_pat(CONS = {cons, P, PH, PT}, ListTy, TEnv, VEnv) ->
    %% TODO: Return non-empty list type as upper bound.
    case expect_list_type(normalize(ListTy, TEnv), dont_allow_nil_type, TEnv) of
        any ->
            VEnv2 = add_any_types_pat(PH, VEnv),
            TailTy = normalize(type(union, [ListTy, type(nil)]), TEnv),
            {_TailPatTy, _TauUBound, VEnv3, Cs} = add_type_pat(PT, TailTy, TEnv, VEnv2),
            {type(none), ListTy, VEnv3, Cs};
        {elem_ty, ElemTy, Cs1} ->
            {_PatTy1, _UBound1, VEnv2, Cs2} = add_type_pat(PH, ElemTy, TEnv, VEnv),
            TailTy = normalize(type(union, [ListTy, type(nil)]), TEnv),
            {_PatTy2, _Ubound2, VEnv3, Cs3} = add_type_pat(PT, TailTy, TEnv, VEnv2),
            {type(none), ListTy, VEnv3, constraints:combine([Cs1, Cs2, Cs3])};
        {type_error, _Ty} ->
            throw({type_error, cons_pat, P, CONS, ListTy})
    end;
add_type_pat(String = {string, P, _}, Ty, TEnv, VEnv) ->
    case subtype(type(string), Ty, TEnv) of
        {true, Cs} ->
            {type(none), type(string), VEnv, Cs};
        false ->
            throw({type_error, pattern, P, String, Ty})
    end;
add_type_pat({bin, _P, BinElements} = Bin, Ty, TEnv, VEnv) ->
    %% Check the size parameters of the bit pattern
    BinTy = gradualizer_bin:compute_type(Bin),
    Cs1 = case subtype(BinTy, Ty, TEnv) of
              {true, Cs0} ->
                  Cs0;
              false ->
                  throw({type_error, Bin, BinTy, Ty})
          end,
    %% Check the elements
    {NewVEnv, Cs} =
        lists:foldl(fun ({bin_element, _, Pat, _Size, _Specifiers} = BinElem,
                         {VEnvAcc, CsAcc}) ->
                            %% Check Pat against the bit syntax type specifiers
                            ElemTy = type_of_bin_element(BinElem),
                            {_PatTy, _UBound, VEnv2, Cs2} =
                                add_type_pat(Pat, ElemTy, TEnv, VEnvAcc),
                            {VEnv2, constraints:combine(CsAcc, Cs2)}
                    end,
                    {VEnv, Cs1},
                    BinElements),
    {type(none), BinTy, NewVEnv, Cs};
add_type_pat({record, P, Record, Fields}, Ty, TEnv, VEnv) ->
    case expect_record_type(Record, Ty, TEnv) of
        type_error -> throw({type_error, record_pattern, P, Record, Ty});
        {ok, Cs1} ->
            {VEnv2, Cs2} = add_type_pat_fields(Fields, Record, TEnv, VEnv),
            {type(none), Ty, VEnv2, constraints:combine(Cs1, Cs2)}
    end;
add_type_pat({map, _, _} = MapPat, {var, _, Var} = TyVar, _TEnv, VEnv) ->
    %% FIXME this is a quite rudimentary implementation
    %% - variables from the map pattern become any()
    %% - the contraint could contain the map keys
    Cs = constraints:add_var(
           Var, constraints:upper(Var, type(map, any))),
    {type(none), TyVar, add_any_types_pat(MapPat, VEnv), Cs};
add_type_pat({map, _, _} = MapPat, ?type(term), TEnv, VEnv) ->
    %% TODO instead of implemented an expect_map_type properly
    %% just handle the one missing case here
    %% expect_map_type(term()) would return #{term() => term()}
    AllMapsTy = type(map, [type(map_field_assoc, [type(term), type(term)])]),
    add_type_pat(MapPat, AllMapsTy, TEnv, VEnv);
add_type_pat({map, _P, PatAssocs}, {type, _, map, MapTyAssocs} = MapTy, TEnv, VEnv) ->
    %% Check each Key := Value and binds vars in Value.
    {NewVEnv, Css} =
        lists:foldl(fun ({map_field_exact, _, Key, ValuePat}, {VEnvIn, CsAcc}) ->
                            case add_type_pat_map_key(Key, MapTyAssocs, TEnv, VEnvIn) of
                                {ok, ValueTy, Cs1} ->
                                    {_ValPatTy, _ValUBound, VEnvOut, Cs2} =
                                        add_type_pat(ValuePat, ValueTy, TEnv, VEnvIn),
                                    {VEnvOut, [Cs1, Cs2 | CsAcc]};
                                error ->
                                    throw({type_error, badkey, Key, MapTy})
                            end
                    end,
                    {VEnv, []},
                    PatAssocs),
    {type(none), MapTy, NewVEnv, constraints:combine(Css)};
add_type_pat({match, _, Pat1, {var, _, _Var}=Pat2}, Ty, TEnv, VEnv) ->
    %% Refine using Pat1 first to be able to bind Pat2 to a refined type.
    {PatTy1, Ty1, VEnv1, Cs2} = add_type_pat(Pat1, Ty, TEnv, VEnv),
    {PatTy2, Ty2, VEnv2, Cs1} = add_type_pat(Pat2, Ty1, TEnv, VEnv1),
    {GlbTy, Cs3} = glb(PatTy1, PatTy2, TEnv),
    {GlbTy, Ty2, VEnv2, constraints:combine([Cs1, Cs2, Cs3])};
add_type_pat({match, _, Pat1, Pat2}, Ty, TEnv, VEnv) ->
    %% Use the refined type of Pat2 to bind vars in Pat1.
    {PatTy1, Ty1, VEnv1, Cs1} = add_type_pat(Pat2, Ty, TEnv, VEnv),
    {PatTy2, Ty2, VEnv2, Cs2} = add_type_pat(Pat1, Ty1, TEnv, VEnv1),
    {GlbTy, Cs3} = glb(PatTy1, PatTy2, TEnv),
    {GlbTy, Ty2, VEnv2, constraints:combine([Cs1, Cs2, Cs3])};
add_type_pat({op, _, '++', Pat1, Pat2}, Ty, TEnv, VEnv) ->
    {_, _, VEnv1, Cs1} = add_type_pat(Pat1, Ty, TEnv, VEnv),
    {_, _, VEnv2, Cs2} = add_type_pat(Pat2, Ty, TEnv, VEnv1),
    {type(none), Ty, VEnv2, constraints:combine(Cs1,Cs2)};
add_type_pat(OpPat = {op, _Anno, _Op, _Pat1, _Pat2}, Ty, TEnv, VEnv) ->
    %% Operator patterns are evaluated at compile-time by the compiler.
    %% So we simply evaluate them and check the type of the resulting value.
    %% This simplified situations like this: suppose Ty = non_neg_integer() and
    %% the pattern is 12 - 13. Is this pattern of the correct type? It is not
    %% enough to just check the two arguments to the operator (which would say that
    %% the pattern has the correct type) but we also need to check that the left
    %% argument is not smaller than the right argument. But instead of implementing
    %% such a check, we simply evaluate the pattern as an expression.
    add_type_pat_literal(OpPat, Ty, TEnv, VEnv);
add_type_pat(OpPat = {op, _Anno, _Op, _Pat}, Ty, TEnv, VEnv) ->
    add_type_pat_literal(OpPat, Ty, TEnv, VEnv);
add_type_pat(Pat, {ann_type, _, [_, Ty]}, TEnv, VEnv) ->
    add_type_pat(Pat, Ty, TEnv, VEnv);

add_type_pat(Pat, Ty, _TEnv, _VEnv) ->
    throw({type_error, pattern, element(2, Pat), Pat, Ty}).

add_type_pat_literal(Pat, Ty, TEnv, VEnv) ->
    case erl_eval:partial_eval(Pat) of
        Pat ->
            %% illegal, non-constant pattern
            %% should not appear in compilable code
            throw({illegal_pattern, Pat});
        Literal ->
            try
                add_type_pat(Literal, Ty, TEnv, VEnv)
            catch
                _TypeError ->
                    %% throw with the original pattern
                    throw({type_error, operator_pattern, Pat, Ty})
            end
    end.

add_type_pat_fields([], _, _TEnv, VEnv) ->
    ret(VEnv);
add_type_pat_fields([{record_field, Anno, {atom, _, _} = FieldWithAnno, Pat}|Fields],
                    Record, TEnv, VEnv) ->
    Rec = get_record_fields(Record, Anno, TEnv),
    FieldTy = get_rec_field_type(FieldWithAnno, Rec),
    {_TyPat, _UBound, VEnv2, Cs1} = add_type_pat(Pat, FieldTy, TEnv, VEnv),
    {VEnv3, Cs2} = add_type_pat_fields(Fields, Record, TEnv, VEnv2),
    {VEnv3, constraints:combine(Cs1, Cs2)};
add_type_pat_fields([{record_field, _, {var, _, '_'}, _Pat}|Fields],
                    Record, TEnv, VEnv) ->
    %% TODO check Pat against type of unassigned fields
    {VEnv2, Cs1} = {VEnv, constraints:empty()},

    {VEnv3, Cs2} = add_type_pat_fields(Fields, Record, TEnv, VEnv2),
    {VEnv3, constraints:combine(Cs1, Cs2)}.

%% Given a pattern for a key, finds the matching association in the map type and
%% returns the value type. Returns 'error' if the key is not valid in the map.
-spec add_type_pat_map_key(Key         :: gradualizer_type:abstract_pattern(),
                           MapTyAssocs :: [{type, erl_anno:anno(),
                                            map_field_exact | map_field_assoc,
                                            [type()]}] | any,
                           TEnv        :: #tenv{},
                           VEnv        :: #{atom() => type()}
                          ) -> {ok, ValueTy :: type(), constraints:constraints()} |
                               error.
add_type_pat_map_key(_Key, any, _TEnv, _VEnv) ->
    {ok, type(any), constraints:empty()};
add_type_pat_map_key(Key, [{type, _, AssocTag, [KeyTy, ValueTy]} | MapAssocs], TEnv, VEnv)
  when AssocTag == map_field_exact; AssocTag == map_field_assoc ->
    try add_type_pat(Key, KeyTy, TEnv, VEnv) of
        {_, _, VEnv, Cs} ->
            %% No free vars in Key, so no new variable binds
            {ok, ValueTy, Cs}
    catch _TypeError ->
        add_type_pat_map_key(Key, MapAssocs, TEnv, VEnv)
    end;
add_type_pat_map_key(_Key, [], _TEnv, _VEnv) ->
    %% Key is not defined in this map type.
    error.

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].


-spec add_any_types_pats([gradualizer_type:abstract_pattern()], VEnv :: map()) ->
                             NewVEnv :: map().
add_any_types_pats([], VEnv) ->
    VEnv;
add_any_types_pats([Pat|Pats], VEnv) ->
    add_any_types_pats(Pats, add_any_types_pat(Pat, VEnv)).

-spec add_any_types_pat(gradualizer_type:abstract_pattern(), VEnv :: map()) ->
                            NewVEnv :: map().
add_any_types_pat({atom, _, _}, VEnv) ->
    VEnv;
add_any_types_pat({integer, _, _}, VEnv) ->
    VEnv;
add_any_types_pat({char, _, _}, VEnv) ->
    VEnv;
add_any_types_pat({float, _, _}, VEnv) ->
    VEnv;
add_any_types_pat({match, _, P1, P2}, VEnv) ->
    add_any_types_pats([P1, P2], VEnv);
add_any_types_pat({cons, _, Head, Tail}, VEnv) ->
    add_any_types_pats([Head, Tail], VEnv);
add_any_types_pat({string, _, _}, VEnv) ->
    VEnv;
add_any_types_pat({nil, _}, VEnv) ->
    VEnv;
add_any_types_pat({tuple, _, Pats}, VEnv) ->
    add_any_types_pats(Pats, VEnv);
add_any_types_pat({record, _, _RecName, Fields}, VEnv) ->
    add_any_types_pats(
      [Value || {record_field, _, _Name, Value} <- Fields], VEnv);
add_any_types_pat({map, _, Fields}, VEnv) ->
    add_any_types_pats(
      lists:flatmap(
        fun({Tag, _, Name, Value})
              when Tag =:= map_field_assoc; Tag =:= map_field_exact ->
                [Name, Value]
        end, Fields),
      VEnv);
add_any_types_pat({bin, _, BinElements}, VEnv) ->
    lists:foldl(fun ({bin_element, _, Pat, _Size, _Specifiers}, VEnv1) ->
                        %% TODO: Ideally, we should use the bit specifiers to
                        %% add types to the variables in Pat. But we don't
                        %% have access to the TEnv here, so we cannot call
                        %% add_type_pat. Perhaps we should change that.
                        add_any_types_pat(Pat, VEnv1)
                    end,
                    VEnv,
                    BinElements);
add_any_types_pat({var, _, '_'}, VEnv) ->
    VEnv;
add_any_types_pat({var, _, A}, VEnv) ->
    case VEnv of
        #{A := _} ->
            VEnv;
        _ ->
            VEnv#{ A => type(any) }
    end;
add_any_types_pat({op, _, '++', _Pat1, Pat2}, VEnv) ->
    %% Pat1 cannot contain any variables so there is no need to traverse it.
    add_any_types_pat(Pat2, VEnv);
add_any_types_pat({op, _, _Op, _Pat1, _Pat2}, VEnv) ->
    %% These patterns cannot contain variables.
    VEnv;
add_any_types_pat({op, _, _Op, _Pat}, VEnv) ->
    %% Cannot contain variables.
    VEnv.


%% Get type from specifiers in a bit syntax, e.g. <<Foo/float-little>>
-spec type_of_bin_element({bin_element,
                           Anno       :: erl_anno:anno(),
                           Expr       :: gradualizer_type:abstract_expr(),
                           Size       :: non_neg_integer() |
                                         default,
                           Specifiers :: [atom() | {unit, pos_integer()}] |
                                         default}) -> type().
type_of_bin_element({bin_element, Anno, Expr, Size, default}) ->
    type_of_bin_element({bin_element, Anno, Expr, Size, []});
type_of_bin_element({bin_element, _P, Expr, _Size, Specifiers}) ->
    %% String literal is syntactic sugar for multiple char literals,
    IsStringLiteral = case Expr of
                          {string, _, _} -> true;
                          _              -> false
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
                                    not IsStringLiteral ->
                                        {true, type(integer)}
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
        [] ->
            %% <<X>>
            type(integer);
        [T] ->
            T
    end.


%%% Helper functions

type(Name, Args) ->
    {type, erl_anno:new(0), Name, Args}.

type(Name) -> type(Name, []).

type_var(Name) ->
    {var, erl_anno:new(0), Name}.

return(X) ->
    { X, #{}, constraints:empty() }.

verbose(Env, Fmt, Args) ->
    Env#env.verbose andalso io:format(Fmt, Args).

is_power_of_two(0) -> false;
is_power_of_two(1) -> true;
is_power_of_two(N) when N rem 2 == 0 ->
    is_power_of_two(N div 2);
is_power_of_two(_) -> false.

union_var_binds(VB1, VB2, TEnv) ->
    union_var_binds([VB1, VB2], TEnv).

%% This function has been identified as a bottleneck.
%% Without tail recursion, the gradualizer would hang when self-gradualizing
%% when called from add_type_pat/4, the clause where the type is a union type.
union_var_binds([], _) ->
    #{};
union_var_binds(VarBindsList, TEnv) ->
    % TODO: Don't drop the constraints
    Glb = fun(_K, Ty1, Ty2) -> {Ty, _Cs} = glb(Ty1, Ty2, TEnv), Ty end,
    union_var_binds_help(VarBindsList, Glb).

%% Tail recursive helper.
union_var_binds_help([VB1, VB2 | Rest], Glb) ->
    VB = gradualizer_lib:merge_with(Glb, VB1, VB2),
    union_var_binds_help([VB | Rest], Glb);
union_var_binds_help([VB], _) -> VB.

add_var_binds(VEnv, VarBinds, TEnv) ->
    % TODO: Don't drop the constraints
    Glb = fun(_K, Ty1, Ty2) -> {Ty, _C} = glb(Ty1, Ty2, TEnv), Ty end,
    gradualizer_lib:merge_with(Glb, VEnv, VarBinds).

get_rec_field_type(FieldWithAnno, RecFields) ->
    %% The first field is the second element of the tuple - so start from 2
    {_Index, Ty} = get_rec_field_index_and_type(FieldWithAnno, RecFields, 2),
    Ty.

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
    throw({undef, record_field, FieldWithAnno}).

%% Helper for finding the return type of record_info/2
-spec get_record_info_type(erl_parse:abstract_expr(), #tenv{}) -> type().
get_record_info_type({call, Anno, {atom, _, record_info},
                            [{atom, _, fields}, {atom, _, RecName}]}, TEnv) ->
    Fields = get_record_fields(RecName, Anno, TEnv),
    Names = [Name || {typed_record_field, {record_field, _, Name, _}, _Ty} <- Fields],
    type(list, [type(union, Names)]);
get_record_info_type({call, Anno, {atom, _, record_info},
                            [{atom, _, size}, {atom, _, RecName}]}, TEnv) ->
    Fields = get_record_fields(RecName, Anno, TEnv),
    {integer, erl_anno:new(0), length(Fields) + 1};
get_record_info_type(Expr, _TEnv) ->
    throw({illegal_record_info, Expr}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec type_check_forms(list(), proplists:proplist()) -> list().
type_check_forms(Forms, Opts) ->
    StopOnFirstError = proplists:get_bool(stop_on_first_error, Opts),
    CrashOnError = proplists:get_bool(crash_on_error, Opts),

    {ok, _} = application:ensure_all_started(gradualizer),
    proplists:get_bool(prelude, Opts) andalso gradualizer_db:import_prelude(),
    gradualizer_db:import_extra_specs(proplists:get_all_values(specs_override, Opts)),

    ParseData =
        collect_specs_types_opaques_and_functions(Forms),
    Env = create_env(ParseData, Opts),
    ?verbose(Env, "Checking module ~p~n", [ParseData#parsedata.module]),
    AllErrors =
        lists:foldr(
          fun (Function, Errors) when Errors =:= [];
                                      not StopOnFirstError ->
                        try type_check_function(Env, Function) of
                            {_VarBinds, _Cs} ->
                                Errors
                        catch
                            Throw ->
                                % Useful for debugging
                                % io:format("~p~n", [erlang:get_stacktrace()]),
                                if
                                    CrashOnError ->
                                        io:format("Crashing...~n"),
                                        erlang:raise(throw, Throw, erlang:get_stacktrace());
                                    not CrashOnError ->
                                        [Throw | Errors]
                                end;
                            error:Error ->
                                %% A hack to hide the (very large) #env{} in
                                %% error stacktraces. TODO: Add an opt for this.
                                Trace = case erlang:get_stacktrace() of
                                    [{M, F, [#env{}|Args], Pos} | RestTrace] ->
                                        [{M, F, ['*environment excluded*'|Args], Pos} | RestTrace];
                                    Trace0 ->
                                        Trace0
                                end,
                                erlang:raise(error, Error, Trace)
                        end;
              (_Function, Errors) ->
                  Errors
          end, [], ParseData#parsedata.functions),
    lists:reverse(AllErrors).

create_env(#parsedata{module    = Module
                     ,specs     = Specs
                     ,functions = Funs
                     ,types     = Types
                     ,opaques   = Opaques
                     ,records   = Records
                     ,imports   = Imports
                     }, Opts) ->
    FEnv = create_fenv(Specs, Funs),
    TEnv = create_tenv(Module, Types ++ Opaques, Records),
    Imported = maps:from_list([{{F, A}, M} || {M, F, A} <- Imports]),
    #env{fenv = FEnv,
         tenv = TEnv,
         imported = Imported,
         %% Store some type checking options in the environment
         infer = proplists:get_bool(infer, Opts),
         verbose = proplists:get_bool(verbose, Opts)}.

create_tenv(Module, TypeDefs, RecordDefs) ->
    TypeMap =
        maps:from_list([begin
                            Id       = {Name, length(Vars)},
                            Params   = [VarName || {var, _, VarName} <- Vars],
                            {Id, {Params, Body}}
                        end || {Name, Body, Vars} <- TypeDefs]),
    RecordMap =
        maps:from_list([{Name, lists:map(fun absform:normalize_record_field/1,
                                         Fields)}
                            || {Name, Fields} <- RecordDefs]),
    #tenv{module  = Module,
          types   = TypeMap,
          records = RecordMap
         }.

create_fenv(Specs, Funs) ->
% We're taking advantage of the fact that if a key occurrs more than once
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

%% TODO: Move these functions to gradualizer or gradualizer_fmt.
print_errors(Errors, Opts) ->
    [print_error(Error, Opts) || Error <- Errors],
    ok.

print_error(Error, Opts) ->
    File = proplists:get_value(filename, Opts),
    FmtLoc = proplists:get_value(fmt_location, Opts, verbose),
    case File of
        undefined -> ok;
        _ when FmtLoc =:= brief -> io:format("~s:", [File]);
        _  -> io:format("~s: ", [File])
    end,
    handle_type_error(Error, Opts).

handle_type_error(Error, Opts) ->
    io:put_chars(gradualizer_fmt:format_type_error(Error, Opts)).

line_no(Expr) ->
    erl_anno:line(element(2, Expr)).

-spec gen_partition(pos_integer(), list(tuple()),
                    fun((tuple()) -> {pos_integer(), term()} | false)) ->
                           tuple().

gen_partition(N,List, Fun) ->
    paux(List, Fun, erlang:list_to_tuple(lists:duplicate(N,[]))).
paux([], _Fun, Tuple) ->
    Tuple;
paux([Elem|List], Fun, Tuple) ->
    case Fun(Elem) of
        {I, Item} ->
            paux(List, Fun, erlang:setelement(I, Tuple, [Item | erlang:element(I,Tuple)]));
        false ->
            paux(List, Fun, Tuple)
    end.
