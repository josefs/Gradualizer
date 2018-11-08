-module(typechecker).

-include("typelib.hrl").

-ifdef(OTP_RELEASE).
-compile([{nowarn_deprecated_function,{erlang,get_stacktrace,0}}]).
-endif.

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

-type type() :: erl_parse:abstract_type().

-compile([export_all]).

%% Data collected from epp parse tree
-record(parsedata, {
	  module             :: atom(),
	  export_all = false :: boolean(),
	  exports    = []    :: [{atom(), integer()}],
	  specs      = []    :: list(),
	  types      = []    :: list(),
	  opaques    = []    :: list(),
	  records    = []    :: list(),
	  functions  = []    :: list()
	 }).

-type typed_record_field() :: {typed_record_field,
                               {record_field, erl_anno:anno(), Name :: {atom, erl_anno:anno(), atom()},
                                DefaultValue :: erl_parse:abstract_expr()},
     			        Type :: type()}.

%% Type environment, passed around while comparing compatible subtypes
-record(tenv, {types = #{} :: #{{Name :: atom(), arity()} => {Params :: [atom()],
							  Body :: type()}},
	       records = #{} :: #{Name :: atom()            => [typed_record_field()]}
	      }).

%%% The environment passed around during typechecking.
-record(env, {fenv  = #{}
	     ,venv  = #{}
	     ,tenv          :: #tenv{}
	     ,infer = false :: boolean()
	     %,tyvenv = #{}
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

% The first argument is a "compatible subtype" of the second.

-spec subtype(type(), type(), #tenv{}) -> {true, any()} | false.
subtype(Ty1, Ty2, TEnv) ->
    try compat(Ty1, Ty2, sets:new(), TEnv) of
	{_Memoization, Constraints} ->
	    {true, Constraints}
    catch
	nomatch ->
	    false
    end.

subtypes([], [], _TEnv) ->
    {true, constraints:empty()};
subtypes([Ty1|Tys1], [Ty2|Tys2], TEnv) ->
    case subtype(Ty1, Ty2, TEnv) of
	false -> false;
	{true, C1} ->
	    case subtypes(Tys1, Tys2, TEnv) of
		false -> false;
		{true, C2} ->
		    {true, constraints:combine(C1,C2)}
	    end
    end.

%% Checks is a type is a subtype of at least one of the types in a list.
%% Used when checking intersection types.
any_subtype(Ty, Tys, TEnv) when not is_list(Tys) ->
    any_subtype(Ty, [Tys], TEnv);
any_subtype(_Ty, [], _TEnv) ->
    false;
any_subtype(Ty, [Ty1|Tys], TEnv) ->
    case subtype(Ty, Ty1, TEnv) of
	R={true, _} ->
	    R;
	false ->
	    any_subtype(Ty, Tys, TEnv)
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
    Fields1 = get_record_fields(Name, P1, TEnv),
    Fields2 = get_record_fields(Name, P2, TEnv),
    compat_record_fields(Fields1, Fields2, A, TEnv);

compat_ty({type, _, record, _}, {type, _, tuple, any}, A, _TEnv) ->
    ret(A);

%% Lists
compat_ty({type, _, list, [_Ty]}, {type, _, list, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, list, []}, {type, _, list, [_Ty]}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, list, [Ty1]}, {type, _, list, [Ty2]}, A, TEnv) ->
    compat(Ty1, Ty2, A, TEnv);
compat_ty({type, _, nil, []}, {type, _, list, _Any}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, nonempty_list, []}, {type, _, list, _Any}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, nonempty_list, [_Ty]}, {type, _, nonempty_list, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, nonempty_list, []}, {type, _, nonempty_list, [_Ty]}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, nonempty_list, [Ty1]}, {type, _, nonempty_list, [Ty2]}, A, TEnv) ->
    compat(Ty1, Ty2, A, TEnv);
compat_ty({type, _, nonempty_list, [Ty1]}, {type, _, list, [Ty2]}, A, TEnv) ->
    compat(Ty1, Ty2, A, TEnv);
compat_ty({type, _, nonempty_list, [_Ty]}, {type, _, list, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, nil, _, []}, {type, _, maybe_improper_list, [_Ty2]}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, list, [Ty1]}, {type, _, maybe_improper_list, [Ty2]}, A, TEnv) ->
    compat(Ty1, Ty2, A, TEnv);
compat_ty({type, _, nonempty_list, [Ty1]}, {type, _, maybe_improper_list, [Ty2]}, A, TEnv) ->
    compat(Ty1, Ty2, A, TEnv);
%% TODO: improper lists

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
    ret(lists:foldl(fun (Assoc2, As) ->
			    any_type(Assoc2, Assocs1, As, TEnv)
		    end, A, Assocs2));
compat_ty({type, _, AssocTag2, [Key2, Val2]},
	  {type, _, AssocTag1, [Key1, Val1]}, A, TEnv)
	when AssocTag2 == map_field_assoc, AssocTag1 == map_field_assoc;
	     AssocTag2 == map_field_exact, AssocTag1 == map_field_exact;
	     AssocTag2 == map_field_assoc, AssocTag1 == map_field_exact ->
    %% For M1 <: M2, mandatory fields in M2 must be mandatory fields in M1
    {A1, Cs1} = compat_ty(Key2, Key1, A, TEnv),
    {A2, Cs2} = compat_ty(Val1, Val2, A1, TEnv),
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
    {A1, Cs1} = compat_ty(T1, T2, A, TEnv),
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
	compat_ty(Ty, Ty1, A, TEnv)
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

get_record_fields(RecName, Anno, #tenv{records = REnv}) ->
    case typelib:get_module_from_annotation(Anno) of
    	{ok, Module} ->
    	    %% A record type in another module, from an expanded remote type
	    case gradualizer_db:get_record_type(Module, RecName) of
		{ok, TypedRecordFields} ->
		    TypedRecordFields;
		not_found ->
		    throw({undef, record, {Module, RecName}})
	    end;
	none ->
	    %% Local record type
	    case REnv of
	    	#{RecName := Fields} ->
		    Fields;
		_NotFound ->
		    throw({undef, record, RecName})
	    end
    end.

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
        []  -> {type, erl_anno:new(0), none, []};
        [T] -> T;
        Ts  -> {type, erl_anno:new(0), union, Ts}
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
                    throw({undef, user_type, {Module, Name, length(Args)}})
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
                    throw({undef, user_type, {Name, length(Args)}})
            end
    end;
normalize({remote_type, P, [{atom, _, M} = Module, {atom, _, N} = Name, Args]}, TEnv) ->
    case gradualizer_db:get_exported_type(M, N, Args) of
        {ok, T} ->
            normalize(T, TEnv);
        opaque ->
            typelib:annotate_user_types(M, {user_type, P, N, Args});
        not_exported ->
            throw({not_exported, remote_type, {Module, Name, length(Args)}});
        not_found ->
            throw({undef, remote_type, {Module, Name, length(Args)}})
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
    {type, Ann, maybe_improper_list, [{type, Ann, union, Union}]};
expand_builtin_aliases({type, Ann, function, []}) ->
    {type, Ann, 'fun', []};
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
            [{type, erl_anno:new(0), term, []}];
        false ->
            {IntegerTypes, NonIntegerTypes} =
                lists:partition(fun is_int_type/1, Types),
            {Anys, OtherTypes} =
                lists:partition(fun ({type, _, any, []}) -> true;
                                    (_)                  -> false
                                end, NonIntegerTypes),
            merge_int_types(IntegerTypes) ++
                OtherTypes ++ %remove_subtypes(OtherTypes, TEnv) ++
                case Anys of
                    []      -> [];
                    [Any|_] -> [Any]
                end
    end.

%% Keep only types that are not subtypes of any other types in the list.
%remove_subtypes(Types, TEnv) ->
%    remove_subtypes_help(Types, Types, TEnv).
%
%remove_subtypes_help([T|Ts], Types, TEnv) ->
%    case any_type(T, Types -- [T], sets:new(), TEnv) of
%        true -> remove_subtypes_help(Ts, Types, TEnv);
%        false -> [T | remove_subtypes_help(Ts, Types, TEnv)]
%    end;
%remove_subtypes_help([], _Types, _TEnv) ->
%    [].

-spec is_int_type(type()) -> boolean().
is_int_type({type, _, T, _})
  when T == pos_integer; T == non_neg_integer; T == neg_integer;
       T == integer; T == range -> true;
is_int_type({integer, _, _}) -> true;
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

int_max(A, B) when A == pos_inf; B == pos_inf   -> pos_inf;
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
int_type_to_range({type, _, range, [{integer, _, I1},
                                    {integer, _, I2}]})
                                        when I1 =< I2  -> {I1, I2};
int_type_to_range({integer, _, I})                     -> {I, I}.

%% Converts a range back to a type. Creates two types in some cases.
-spec int_range_to_types(int_range()) -> [type()].
int_range_to_types({neg_inf, pos_inf}) ->
    [{type, erl_anno:new(0), integer, []}];
int_range_to_types({neg_inf, -1}) ->
    [{type, erl_anno:new(0), neg_integer, []}];
int_range_to_types({neg_inf, 0}) ->
    [{type, erl_anno:new(0), neg_integer, []}, {integer, erl_anno:new(0), 0}];
int_range_to_types({neg_inf, I}) when I > 0 ->
    [{type, erl_anno:new(0), neg_integer, []},
     {type, erl_anno:new(0), range, [{integer, erl_anno:new(0), 0}
				    ,{integer, erl_anno:new(0), I}]}];
int_range_to_types({I, pos_inf}) when I < -1 ->
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), I}
				    ,{integer, erl_anno:new(0), -1}]},
     {type, erl_anno:new(0), non_neg_integer, []}];
int_range_to_types({-1, pos_inf}) ->
    [{integer, erl_anno:new(0), -1}, {type, erl_anno:new(0), non_neg_integer, []}];
int_range_to_types({0, pos_inf}) ->
    [{type, erl_anno:new(0), non_neg_integer, []}];
int_range_to_types({1, pos_inf}) ->
    [{type, erl_anno:new(0), pos_integer, []}];
int_range_to_types({I, I}) ->
    [{integer, erl_anno:new(0), I}];
int_range_to_types({I, J}) when I < J ->
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), I}
				    ,{integer, erl_anno:new(0), J}]}].

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
            {type, erl_anno:new(0), union, Tys}
    end.

negate_bool_type({atom, P, true}) ->
    {atom, P, false};
negate_bool_type({atom, P, false}) ->
    {atom, P, true};
negate_bool_type(Ty) ->
    Ty.

%% End of subtype help functions

%% Pattern matching on types
%%
%% We sometimes need to pattern match on a type in order to get to
%% its type parameter. One example is the list type, and there are
%% cases where we need to get the type of the elements.

-spec expect_list_type(type(), allow_nil_type | dont_allow_nil_type) ->
          {elem_ty,  type(),   constraints:constraints()}    %% There is exactly one element type
	| {elem_tys, [type()], constraints:constraints()}  %% A union can give rise to multiple elem types
	| any                   %% If we don't know the element type
	| {type_error, type()}. %% If the argument is not compatible with lists

expect_list_type({type, _, T, []}, _)
  when T == 'list' orelse T == 'any' orelse
       T == 'nonempty_list' orelse T == 'maybe_improper_list' ->
    any;
expect_list_type({type, _, T, [ElemTy]}, _)
  when T == 'list' orelse T == 'nonempty_list' ->
    {elem_ty, ElemTy, constraints:empty()};
expect_list_type({type, _, maybe_improper_list, [ElemTy, _]}, _) ->
    {elem_ty, ElemTy, constraints:empty()};
expect_list_type({type, _, nil, []}, allow_nil_type) ->
    any;
expect_list_type({type, _, string, []}, _) ->
    {elem_ty, {type, erl_anno:new(0), char, []}, constraints:empty()};
expect_list_type({ann_type, _, [_, Ty]}, N) ->
    expect_list_type(Ty, N);
expect_list_type(Union = {type, _, union, UnionTys}, N) ->
    {Tys, Cs} = expect_list_union(UnionTys, [], constraints:empty(), no_any, N),
    case Tys of
	[] ->
	    {type_error, Union};
	[Ty] ->
	    {elem_ty, Ty, Cs};
	_ ->
	    {elem_tys, Tys, Cs}
    end;
expect_list_type({var, _, Var}, _) ->
    TyVar = new_type_var(),
    {elem_ty
    ,{var, erl_anno:new(0), TyVar}
    ,constraints:add_var(TyVar,
      constraints:upper(Var, {type, erl_anno:new(0), list, [TyVar]}))
    };
expect_list_type(Ty, _) ->
    {type_error, Ty}.


expect_list_union([Ty|Tys], AccTy, AccCs, Any, N) ->
    case expect_list_type(Ty, N) of
	{type_error, _} ->
	    expect_list_union(Tys, AccTy, AccCs, Any, N);
	any ->
	    expect_list_union(Tys
			     ,[{type, erl_anno:new(0), any, []} | AccTy]
			     ,AccCs
			     ,any
			     ,N);
	{elem_ty, NTy, Cs} ->
	    expect_list_union(Tys
			     ,[NTy | AccTy]
			     ,constraints:combine(Cs, AccCs)
			     ,Any
			     ,N);
	{elem_tys, NTys, Cs} ->
	    expect_list_union(Tys
			     ,NTys ++ AccTy
			     ,constraints:combine(Cs, AccCs)
			     ,Any
			     ,N)
    end;
expect_list_union([], AccTy, AccCs, any, _N) ->
    {[{type, erl_anno:new(0), any, []} | AccTy], AccCs};
expect_list_union([], AccTy, AccCs, _NoAny, _N) ->
    {AccTy, AccCs}.

expect_tuple_type({type, _, tuple, any}, _N) ->
    any;
expect_tuple_type({type, _, tuple, Tys}, N) when length(Tys) == N ->
    {elem_ty, Tys, constraints:empty()};
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
		,constraints:upper(Var, {type, erl_anno:new(0), tuple, TyVars})
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
    {[ lists:duplicate(N, {type, erl_anno:new(0), any, []}) | AccTy], AccCs};
expect_tuple_union([], AccTy, AccCs, _NoAny, _N) ->
    {AccTy, AccCs}.

-spec expect_fun_type(type()) -> any
			       | {type_error, type()}
			       | {fun_ty, type(), type(), constraints:constraints()}
			       | {fun_ty_any_args, type(), constraints:constraints()}
			       | {fun_ty_intersection, [type()], constraints:constraints()}
			       | {fun_ty_union, [any()], constraints:constraints()}
			       .
expect_fun_type({type, _, bounded_fun, [Ft, Fc]}) ->
    case expect_fun_type(Ft) of
	{fun_ty, ArgsTy, ResTy, Cs} ->
	    {fun_ty, ArgsTy, ResTy, constraints:combine(Cs, constraints:convert(Fc))};
	{fun_ty_any_args, ResTy, Cs} ->
	    {fun_ty_any_args, ResTy, constraints:combine(Cs, constraints:convert(Fc))};
	{fun_ty_intersection, Tys, Cs} ->
	    {fun_ty_intersection, Tys, constraints:combine(Cs, constraints:convert(Fc))};
	{fun_ty_union, Tys, Cs} ->
	    {fun_ty_union, Tys, constraints:combine(Cs, constraints:convert(Fc))};
	Err ->
	    Err
    end;
expect_fun_type({type, _, 'fun', [{type, _, product, ArgsTy}, ResTy]}) ->
    {fun_ty, ArgsTy, ResTy, constraints:empty()};
expect_fun_type({type, _, 'fun', []}) ->
    any;
expect_fun_type({type, _, 'fun', [{type, _, any}, ResTy]}) ->
    {fun_ty_any_args, ResTy, constraints:empty()};
expect_fun_type(Tys) when is_list(Tys) ->
    case expect_intersection_type(Tys) of
	{type_error, _} ->
	    {type_error, Tys};
	[Ty] ->
	    Ty;
	Tyss ->
	    {fun_ty_intersection, Tyss, constraints:empty()}
    end;
expect_fun_type({type, _, union, UnionTys} = Union) ->
    case expect_fun_type_union(UnionTys) of
	[] ->
	    {type_error, Union};
	[Ty] ->
	    Ty;
	Tys ->
	    {fun_ty_union, Tys, constraints:empty()}
    end;
expect_fun_type({ann_type, _, [_, Ty]}) ->
    expect_fun_type(Ty);
expect_fun_type({var, _, Var}) ->
    ResTy = new_type_var(),
    {fun_ty_any_args, {var, erl_anno:new(0), ResTy}
    ,constraints:add_var(Var,
       constraints:upper(ResTy,
	 {type, erl_anno:new(0), 'fun', [{type, erl_anno:new(0), any}
					,{var,  erl_anno:new(0), ResTy}]}))};
expect_fun_type({type, _, any, []}) ->
    any;
expect_fun_type(Ty) ->
    {type_error, Ty}.

expect_intersection_type([]) ->
    [];
expect_intersection_type([FunTy|Tys]) ->
    case expect_fun_type(FunTy) of
	Err = {type_error, _} ->
	    Err;
	Ty ->
	    case expect_intersection_type(Tys) of
		Err = {type_error,_} ->
		    Err;
		Tyss ->
		    [Ty|Tyss]
	    end
    end.

expect_fun_type_union([]) ->
    [];
expect_fun_type_union([Ty|Tys]) ->
    case expect_fun_type(Ty) of
	{type_error, _} ->
	    expect_fun_type_union(Tys);
	TyOut ->
	    [TyOut | expect_fun_type_union(Tys)]
    end.

expect_record_type(Record, {type, _, record, [{atom, _, Name}]}, _TEnv) ->
    if Record == Name ->
         {ok, constraints:empty()};
       true ->
         type_error
    end;
expect_record_type(_Record, {type, _, tuple, any}, _TEnv) ->
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
type_check_expr(Env, {var, P, Var}) ->
    case catch maps:get(Var, Env#env.venv) of
	{'EXIT', {{badkey, _}, _}} ->
	    throw({unknown_variable, P, Var});
	Ty ->
	    return(Ty)
    end;
type_check_expr(Env, {match, _, Pat, Expr}) ->
    {Ty, VarBinds, Cs} = type_check_expr(Env, Expr),
    {Env2, Cs2} = add_type_pat(Pat, Ty, Env#env.tenv, VarBinds),
    {Ty, Env2, constraints:combine(Cs,Cs2)};
type_check_expr(Env, {'if', _, Clauses}) ->
    infer_clauses(Env, Clauses);
type_check_expr(Env, {'case', _, Expr, Clauses}) ->
    {_ExprTy, VarBinds, Cs1} = type_check_expr(Env, Expr),
    VEnv = add_var_binds(Env#env.venv, VarBinds),
    {Ty, VB, Cs2} = infer_clauses(Env#env{ venv = VEnv}, Clauses),
    {Ty, VB, constraints:combine(Cs1, Cs2)};
type_check_expr(Env, {tuple, P, TS}) ->
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
    { InferredTy, union_var_binds(VarBindsList), constraints:combine(Css) };
type_check_expr(Env, {cons, _, Head, Tail}) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Head),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Tail),
    VB = union_var_binds([VB1, VB2]),
    Cs = constraints:combine([Cs1, Cs2]),
    case {Ty1, Ty2} of
	{{type, _, any, []}, {type, _, any, []}} when not Env#env.infer ->
	    %% No type information to propagate
	    {{type, erl_anno:new(0), any, []}, VB, Cs};
	{_, {type, _, any, []}} ->
	    %% Propagate type information from head
	    {{type, erl_anno:new(0), nonempty_list, [Ty1]}, VB, Cs};
	{_, _} ->
	    %% Propagate type information from tail, which must be a list type
	    TailElemTy =
		case Ty2 of
		    {type, _, nil, []} ->
			{type, erl_anno:new(0), none, []};
		    {type, _, L, []} when L == list;
					  L == nonempty_list ->
			{type, erl_anno:new(0), any, []};
		    {type, _, L, [TailElemTy0]} when L == list;
						     L == nonempty_list ->
			TailElemTy0;
		    _ ->
			throw({type_error, list, 0, Ty2})
			%% We throw a type error here because Tail is not of type list
			%% (nor is it of type any()).
			%% TODO: Improper list?
		end,
	    ElemTy = normalize({type, erl_anno:new(0), union, [Ty1, TailElemTy]},
			       Env#env.tenv),
	    {{type, erl_anno:new(0), nonempty_list, [ElemTy]}, VB, Cs}
    end;
type_check_expr(Env, {bin, _, BinElements}) ->
    %% <<Expr:Size/TypeSpecifierList, ...>>
    TEnv = Env#env.tenv,
    VarBindAndCsList =
	lists:map(fun ({bin_element, P, Expr, _Size, Specifiers}) ->
			  {Ty, VB, Cs1} = type_check_expr(Env, Expr),
			  %% Check Ty against the list of specifiers
			  SpecTy = bit_specifier_list_to_type(Specifiers),
			  case subtype(Ty, SpecTy, TEnv) of
				{true, Cs2} ->
				    {VB, constraints:combine(Cs1, Cs2)};
				false ->
				    throw({type_error, bit_type, P, Ty, SpecTy})
			  end
		  end,
		  BinElements),
    {VarBinds, Css} = lists:unzip(VarBindAndCsList),
    RetTy = if
		Env#env.infer ->
		    %% TODO: Infer the size parameters of the bitstring
		    {type, erl_anno:new(0), binary,
		     [{integer, erl_anno:new(0), 0},
		      {integer, erl_anno:new(0), 1}]};
		not Env#env.infer ->
		    {type, erl_anno:new(0), any, []}
	    end,
    {RetTy,
     union_var_binds(VarBinds),
     constraints:combine(Css)};
type_check_expr(Env, {call, P, Name, Args}) ->
    {FunTy, VarBinds1, Cs1} = type_check_fun(Env, Name, length(Args)),
    {ResTy, VarBinds2, Cs2} = type_check_call_ty(Env, expect_fun_type(FunTy), Args
						,{Name, P, FunTy}),
    {ResTy, union_var_binds(VarBinds1, VarBinds2), constraints:combine(Cs1, Cs2)};

type_check_expr(Env, {lc, _, Expr, Qualifiers}) ->
    type_check_lc(Env, Expr, Qualifiers);
type_check_expr(Env, {block, _, Block}) ->
    type_check_block(Env, Block);

% Don't return the type of anything other than something
% which ultimately comes from a function type spec.
type_check_expr(#env{infer = false}, {string, _, _}) ->
    return({type, erl_anno:new(0), any, []});
type_check_expr(#env{infer = false}, {nil, _}) ->
    return({type, erl_anno:new(0), any, []});
type_check_expr(#env{infer = false}, {atom, _, _Atom}) ->
    return({type, erl_anno:new(0), any, []});
type_check_expr(#env{infer = false}, {integer, _, _N}) ->
    return({type, erl_anno:new(0), any, []});
type_check_expr(#env{infer = false}, {float, _, _F}) ->
    return({type, erl_anno:new(0), any, []});
type_check_expr(#env{infer = false}, {char, _, _C}) ->
    return({type, erl_anno:new(0), any, []});

%% When infer = true, we do propagate the types of literals,
%% list cons, tuples, etc.
type_check_expr(#env{infer = true}, {string, _, _}) ->
    return({type, erl_anno:new(0), string, []});
type_check_expr(#env{infer = true}, {nil, _}) ->
    return({type, erl_anno:new(0), nil, []});
type_check_expr(#env{infer = true}, {atom, _, _} = Atom) ->
    return(Atom);
type_check_expr(#env{infer = true}, {integer, _, _N} = Integer) ->
    return(Integer);
type_check_expr(#env{infer = true}, {float, _, _F}) ->
    return({type, erl_anno:new(0), float, []});
type_check_expr(#env{infer = true}, {char, _, _C} = Char) ->
    return(Char);

%% Maps
type_check_expr(Env, {map, _, Assocs}) ->
    type_check_assocs(Env, Assocs);
type_check_expr(Env, {map, _, Expr, Assocs}) ->
    {Ty, VBExpr,   Cs1} = type_check_expr(Env, Expr),
    {_AssocTy, VBAssocs, Cs2} = type_check_assocs(Env, Assocs),
    % TODO: Update the type of the map.
    % TODO: Check the type of the map.
    {Ty, union_var_binds([VBExpr, VBAssocs]), constraints:combine(Cs1, Cs2)};

%% Records
type_check_expr(Env, {record_field, _P, Expr, Record, {atom, _, Field}}) ->
    {VB, Cs} = type_check_expr_in(Env, {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]}, Expr),
    Rec = maps:get(Record, Env#env.tenv#tenv.records),
    Ty = get_rec_field_type(Field, Rec),
    {Ty, VB, Cs};
type_check_expr(Env, {record, _, Expr, Record, Fields}) ->
    RecTy = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    {VB1, Cs1} = type_check_expr_in(Env, RecTy, Expr),
    Rec = maps:get(Record, Env#env.tenv#tenv.records),
    {VB2, Cs2} = type_check_fields(Env, Rec, Fields),
    {RecTy, union_var_binds([VB1, VB2]), constraints:combine(Cs1, Cs2)};
type_check_expr(Env, {record, _, Record, Fields}) ->
    RecTy    = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    Rec      = maps:get(Record, Env#env.tenv#tenv.records),
    {VB, Cs} = type_check_fields(Env, Rec, Fields),
    {RecTy, VB, Cs};
type_check_expr(_Env, {record_index, _, _Record, _Field}) ->
    {{type, erl_anno:new(0), integer, []}, #{}, constraints:empty()};

%% Functions
type_check_expr(Env, {'fun', _, {clauses, Clauses}}) ->
    type_check_fun(Env, Clauses);
type_check_expr(Env, {'fun', P, {function, Name, Arity}}) ->
    case get_type_from_name_arity(Name, Arity, Env#env.fenv, P) of
	AnyType = {type, _, any, []} ->
	    {AnyType, #{}, constraints:empty()};
	BoundedFunTypeList ->
	    {Ty, Cs} = absform:function_type_list_to_fun_types(BoundedFunTypeList),
	    {Ty, #{}, Cs}
    end;
type_check_expr(Env, {'fun', P, {function, M, F, A}}) ->
    case {get_atom(Env, M), get_atom(Env, F), A} of
	{{atom, _, Module}, {atom, _, Function}, {integer, _, Arity}} ->
	    case gradualizer_db:get_spec(Module, Function, Arity) of
		{ok, BoundedFunTypeList} ->
		    {Ty, Cs} = absform:function_type_list_to_fun_types(BoundedFunTypeList),
		    {Ty, #{}, Cs};
		not_found ->
		    throw({call_undef, P, M, F, A})
	    end;
	_ -> %% Not enough information to check the type of the call.
	    {{type, erl_anno:new(0), any, []}, #{}, constraints:empty()}
    end;
type_check_expr(Env, {named_fun, _, FunName, Clauses}) ->
    %% Pick a type for the fun itself, to be used when checking references to
    %% itself inside the fun, e.g. recursive calls.
    FunTy = if
		Env#env.infer ->
		    %% Create a fun type of the correct arity
		    %% on the form fun((_,_,_) -> any()).
		    [{clause, _, Params, _Guards, _Block} | _] = Clauses,
		    Arity = length(Params),
		    create_fun_type(Arity, {type, erl_anno:new(0), any, []});
		not Env#env.infer ->
		    {type, erl_anno:new(0), any, []}
	    end,
    NewEnv = Env#env{ venv = add_var_binds(#{FunName => FunTy}
                                          ,Env#env.venv) },
    type_check_fun(NewEnv, Clauses);

type_check_expr(Env, {'receive', _, Clauses}) ->
    infer_clauses(Env, Clauses);
type_check_expr(Env, {'receive', _, Clauses, _After, Block}) ->
    {TyClauses, VarBinds1, Cs1} = infer_clauses(Env, Clauses),
    {TyBlock,   VarBinds2, Cs2} = type_check_block(Env, Block),
    {normalize({type, erl_anno:new(0), union, [TyClauses, TyBlock]}, Env#env.tenv)
    ,union_var_binds(VarBinds1, VarBinds2)
    ,constraints:combine(Cs1, Cs2)};

%% Operators
type_check_expr(Env, {op, _, '!', Proc, Val}) ->
    % Message passing is always untyped, which is why we discard the types
    {_, VB1, Cs1} = type_check_expr(Env, Proc),
    {_, VB2, Cs2} = type_check_expr(Env, Val),
    {{type, erl_anno:new(0), any, []}
    ,union_var_binds([VB1, VB2])
    ,constraints:combine(Cs1, Cs2)};
type_check_expr(Env, {op, P, 'not', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    case subtype(Ty, {type, P, boolean, []}, Env#env.tenv) of
	{true, Cs2} ->
            NormTy = normalize(Ty, Env#env.tenv),
            {negate_bool_type(NormTy), VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, non_boolean_argument_to_not, P, Ty})
    end;
type_check_expr(Env, {op, P, 'bnot', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    case subtype(Ty, {type, P, integer, []}, Env#env.tenv) of
	{true, Cs2} ->
            {{type, erl_anno:new(0), integer, []}, VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, non_integer_argument_to_bnot, P, Ty})
    end;
type_check_expr(Env, {op, P, '+', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    case subtype(Ty, {type, P, number ,[]}, Env#env.tenv) of
	{true, Cs2} ->
            {Ty, VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, non_number_argument_to_plus, P, Ty})
    end;
type_check_expr(Env, {op, P, '-', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    case subtype(Ty, {type, P, number ,[]}, Env#env.tenv) of
	{true, Cs2} ->
            NormTy = normalize(Ty, Env#env.tenv),
            {negate_num_type(NormTy), VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, non_number_argument_to_minus, P, Ty})
    end;
type_check_expr(Env, {op, P, BoolOp, Arg1, Arg2}) when
      (BoolOp == 'andalso') or (BoolOp == 'and') or
      (BoolOp == 'orelse')  or (BoolOp == 'or') or (BoolOp == 'xor') ->
    type_check_logic_op(Env, BoolOp, P, Arg1, Arg2);
type_check_expr(Env, {op, P, RelOp, Arg1, Arg2}) when
      (RelOp == '=:=') or (RelOp == '==') or
      (RelOp == '=/=') or (RelOp == '/=') or
      % It's debatable whether we should allow comparison between any types
      % but right now it's allowed
      (RelOp == '>=')  or (RelOp == '=<') or
      (RelOp == '>')  or (RelOp == '<') ->
    type_check_rel_op(Env, RelOp, P, Arg1, Arg2);
type_check_expr(Env, {op, P, Op, Arg1, Arg2}) when
      Op == '+' orelse Op == '-' orelse Op == '*' orelse Op == '/' ->
    type_check_arith_op(Env, Op, P, Arg1, Arg2);
type_check_expr(Env, {op, P, Op, Arg1, Arg2}) when
      Op == 'div'  orelse Op == 'rem' orelse
      Op == 'band' orelse Op == 'bor' orelse Op == 'bxor' orelse
      Op == 'bsl'  orelse Op == 'bsr' ->
    type_check_int_op(Env, Op, P, Arg1, Arg2);
type_check_expr(Env, {op, P, Op, Arg1, Arg2}) when
      Op == '++' orelse Op == '--' ->
    type_check_list_op(Env, Op, P, Arg1, Arg2);

%% Exception constructs
%% There is no typechecking of exceptions
type_check_expr(Env, {'catch', _, Arg}) ->
    type_check_expr(Env, Arg);
type_check_expr(Env, {'try', _, Block, CaseCs, CatchCs, AfterBlock}) ->
    {Ty,  VB,   Cs1} = type_check_block(Env, Block),
    Env2 = Env#env{ venv = add_var_binds(VB, Env#env.venv) },
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
		    {type, erl_anno:new(0), any, []};
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
    ParTys = lists:duplicate(Arity, {type, erl_anno:new(0), any, []}),
    {type, erl_anno:new(0), 'fun',
     [{type, erl_anno:new(0), product, ParTys}, RetTy]}.

type_check_fields(Env, Rec, Fields) ->
    UnAssignedFields = get_unassigned_fields(Fields, Rec),
    type_check_fields(Env, Rec, Fields, UnAssignedFields).

type_check_fields(Env, Rec, [{record_field, _, {atom, _, Field}, Expr} | Fields]
		 ,UnAssignedFields) ->
    FieldTy = get_rec_field_type(Field, Rec),
    {VB1, Cs1} = type_check_expr_in(Env, FieldTy, Expr),
    {VB2, Cs2} = type_check_fields(Env, Rec, Fields, UnAssignedFields),
    {union_var_binds([VB1, VB2]), constraints:combine(Cs1,Cs2)};
type_check_fields(Env, Rec, [{record_field, _, {var, _, '_'}, Expr} | Fields]
		 ,UnAssignedFields) ->
    {VB1, Cs1} = type_check_fields(Env, Rec
				  ,[ {record_field, erl_anno:new(0)
				     ,{atom, erl_anno:new(0), Field}, Expr}
				     || Field <- UnAssignedFields]
				  ,should_not_be_inspected),
    {VB2, Cs2} = type_check_fields(Env, Rec, Fields, UnAssignedFields),
    {union_var_binds([VB1, VB2]), constraints:combine(Cs1,Cs2)};
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
			union_var_binds([VB1, VB2])
		end
	end,
    {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
    case subtype(Ty1, {type, P, bool, []}, Env#env.tenv) of
	false ->
	    throw({type_error, boolop, Op, P, Ty1});
	{true, Cs2} ->
	    {Ty2, VB2, Cs3} = type_check_expr(Env#env{ venv = UnionVarBindsSecondArg(Env#env.venv,VB1 )}, Arg2),
	    case subtype(Ty2, {type, P, bool, []}, Env#env.tenv) of
		false ->
		    throw({type_error, boolop, Op, P, Ty2});
		{true, Cs4} ->
		    {normalize({type, erl_anno:new(0), union, [Ty1, Ty2]}, Env#env.tenv)
		    ,union_var_binds([VB1, VB2])
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
				{type, erl_anno:new(0), any, []};
			    {_,{type, _, any, []}} ->
				{type, erl_anno:new(0), any, []};
			    {_,_} ->
				% Return boolean() when both argument types
				% are known, i.e. not any().
				{type, erl_anno:new(0), boolean, []}
			end,
		    {RetType
		    ,union_var_binds([VB1, VB2])
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
	Ty ->
	    {Ty
	    ,union_var_binds([VB1, VB2])
	    ,constraints:combine(Cs1, Cs2)}
    end.

type_check_int_op(Env, Op, P, Arg1, Arg2) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Arg2),

    case compat_arith_type(Ty1,Ty2) of
	false ->
	    throw({type_error, int_error, Op, P, Ty1, Ty2});
	{type, _, Ty, []} when Ty == float orelse Ty == number ->
	    throw({type_error, int_error, Op, P, Ty1, Ty2});
	Ty ->
	    {Ty
	    ,union_var_binds([VB1, VB2])
	    ,constraints:combine(Cs1, Cs2)}
    end.

type_check_list_op(Env, Op, P, Arg1, Arg2) ->
  {Ty1, VB1, Cs1} = type_check_expr(Env, Arg1),
  {Ty2, VB2, Cs2} = type_check_expr(Env, Arg2),

  ListTy = {type, erl_anno:new(0), list, []},

  case {subtype(Ty1, ListTy, Env#env.tenv)
       ,subtype(Ty2, ListTy, Env#env.tenv)} of
    {{true, Cs3}, {true, Cs4}} ->
      {normalize({type, erl_anno:new(0), union, [Ty1, Ty2]}, Env#env.tenv)
      ,union_var_binds([VB1, VB2])
      ,constraints:combine([Cs1, Cs2, Cs3, Cs4])
      };
    {false, _} ->
      throw({type_error, list_op_error, Op, P, Ty1, Arg1});
    {_, false} ->
      throw({type_error, list_op_error, Op, P, Ty2, Arg2})
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
            ,union_var_binds(VarBindsList)
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
    ,union_var_binds(VarBindsList)
    ,constraints:combine([Cs | Css])};
type_check_call_ty(Env, any, Args, _E) ->
    {_Tys, VarBindsList, Css} =
	lists:unzip3(
	  [ type_check_expr(Env, Arg)
	    || Arg <- Args
	  ]),
    {{type, erl_anno:new(0), any, []}
    ,union_var_binds(VarBindsList)
    ,constraints:combine(Css)};
type_check_call_ty(Env, {fun_ty_intersection, Tyss, Cs}, Args, E) ->
    {ResTy, VarBinds, CsI} = type_check_call_ty_intersect(Env, Tyss, Args, E),
    {ResTy, VarBinds, constraints:combine(Cs, CsI)};
type_check_call_ty(Env, {fun_ty_union, Tyss, Cs}, Args, E) ->
    {ResTy, VarBinds, CsI} = type_check_call_ty_union(Env, Tyss, Args, E),
    {ResTy, VarBinds, constraints:combine(Cs, CsI)};
type_check_call_ty(_Env, {type_error, _}, _Args, {Name, P, FunTy}) ->
    throw({type_error, call, P, FunTy, Name}).

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
    {normalize({type, erl_anno:new(0), union, ResTys}, Env#env.tenv),
     union_var_binds(VBs),
     constraints:combine(Css)}.

compat_arith_type(Any = {type, _, any, []}, {type, _, any, []}) ->
    Any;
compat_arith_type(Any = {type, _, any, []}, Ty) ->
    case subtype(Ty, {type, erl_anno:new(0), number, []}, #tenv{}) of
	false ->
	    false;
	_ ->
	    Any
    end;
compat_arith_type(Ty, Any = {type, _, any, []}) ->
    case subtype(Ty, {type, erl_anno:new(0), number, []}, #tenv{}) of
	false ->
	    false;
	_ ->
	    Any
    end;
compat_arith_type(Ty1, Ty2) ->
    TInteger = {type, erl_anno:new(0), integer, []},
    case {subtype(Ty1, TInteger, #tenv{})
	 ,subtype(Ty2, TInteger, #tenv{})} of
	{{true,_},{true,_}} ->
	    TInteger;
	_ ->
        TFloat = {type, erl_anno:new(0), float, []},
	    case {subtype(Ty1, TFloat, #tenv{})
		 ,subtype(Ty2, TFloat, #tenv{})} of
		{{true,_},{true,_}} ->
		    TFloat;
		_ ->
            TNumber = {type, erl_anno:new(0), number, []},
		    case {subtype(Ty1, TNumber, #tenv{})
			 ,subtype(Ty2, TNumber, #tenv{})} of
			{{true,_},{true,_}} ->
			    TNumber;
			_ ->
			    false
		    end
	    end
    end.

type_check_lc(Env, Expr, []) ->
    {Ty, _VB, Cs} = type_check_expr(Env, Expr),
    RetTy = case Ty of
		{type, _, any, []} when not Env#env.infer ->
		    %% No type information to propagate. We don't infer a
		    %% list type of the list comprehension when inference
		    %% is disabled.
		    {type, erl_anno:new(0), any, []};
		_ ->
		    %% Propagate the type information
		    {type, erl_anno:new(0), list, [Ty]}
	    end,
    {RetTy, #{}, Cs};
type_check_lc(Env, Expr, [{generate, P, Pat, Gen} | Quals]) ->
    {Ty,  _,  Cs1} = type_check_expr(Env, Gen),
    case expect_list_type(Ty, allow_nil_type) of
	{elem_ty, ElemTy, Cs} ->
	    {NewEnv, Cs2} = add_type_pat(Pat
					,ElemTy
					,Env#env.tenv
					,Env#env.venv),
	    {TyL, VB, Cs3} = type_check_lc(Env#env{
					     venv = NewEnv
					    }
					  ,Expr, Quals),
	    {TyL, VB, constraints:combine([Cs, Cs1, Cs2, Cs3])};
	any ->
	    {TyL, VB, Cs2} = type_check_lc(Env#env{
					     venv = add_any_types_pat(
						      Pat
						     ,Env#env.venv)
					    }
					  ,Expr, Quals),
	    {TyL, VB, constraints:combine(Cs1,Cs2)};
	{elem_tys, _ElemTys, Cs} ->
	    %% TODO: As a hack, we treat a union type as any, just to
	    %% allow the program to type check.
	    {TyL, VB, Cs2} = type_check_lc(Env#env{
					     venv = add_any_types_pat(
						      Pat
						     ,Env#env.venv)
					    }
					  ,Expr, Quals),
	    {TyL, VB, constraints:combine([Cs,Cs1,Cs2])};
	{type_error, Ty} ->
	    throw({type_error, generator, P, Ty})
    end;
type_check_lc(Env, Expr, [{b_generate, _P, Pat, Gen} | Quals]) ->
    BitStringTy = {type, erl_anno:new(0), bitstring, []},
    {VarBinds1, Cs1} =
        type_check_expr_in(Env, BitStringTy, Gen),
    {NewEnv, Cs2} = add_type_pat(Pat, BitStringTy, Env#env.tenv, Env#env.venv),
    {TyL, VarBinds2, Cs3} = type_check_lc(Env#env{ venv = NewEnv }, Expr, Quals),
    {TyL
    ,union_var_binds(VarBinds1, VarBinds2)
    ,constraints:combine([Cs1, Cs2, Cs3])};
type_check_lc(Env, Expr, [Guard | Quals]) ->
    %% We don't require guards to return a boolean.
    %% This decision is up for debate.
    {_Ty, VarBinds1, Cs1} = type_check_expr(Env, Guard),
    {TyL, VarBinds2, Cs2} = type_check_lc(Env#env{}, Expr, Quals),
    {TyL, union_var_binds(VarBinds1, VarBinds2), constraints:combine(Cs1, Cs2)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checking the type of an expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_check_expr_in(Env, ResTy, Expr) ->
    NormResTy = normalize(ResTy, Env#env.tenv),
    ?throw_orig_type(do_type_check_expr_in(Env, NormResTy, Expr),
                     ResTy, NormResTy).

do_type_check_expr_in(Env, {type, _, any, []}, Expr) ->
    {_Ty, VB, Cs} = type_check_expr(Env, Expr),
    {VB, Cs};
do_type_check_expr_in(Env, Ty, {var, LINE, Var}) ->
    VarTy = maps:get(Var, Env#env.venv),
    case subtype(VarTy, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, tyVar, LINE, Var, VarTy, Ty})
    end;
do_type_check_expr_in(Env, Ty, {match, _, Pat, Expr}) ->
    {VarBinds, Cs} = type_check_expr_in(Env, Ty, Expr),
    {NewVEnv, Cs2} = add_type_pat(Pat, Ty, Env#env.tenv, Env#env.venv),
    {union_var_binds(VarBinds, NewVEnv), constraints:combine(Cs, Cs2)};
do_type_check_expr_in(Env, Ty, I = {integer, LINE, Int}) ->
    case subtype(I, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, int, Int, LINE, Ty})
    end;
do_type_check_expr_in(Env, Ty, {float, LINE, F}) ->
    case subtype({type, LINE, float, []}, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, float, F, LINE, Ty})
    end;
do_type_check_expr_in(Env, Ty, Atom = {atom, LINE, _}) ->
    case subtype(Atom, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, Atom, LINE, Ty})
    end;
do_type_check_expr_in(Env, Ty, Char = {char, LINE, _}) ->
    case subtype({type, erl_anno:new(0), char, []}, Ty, Env#env.tenv) of
        {true, Cs} ->
           {#{}, Cs};
       false ->
           throw({type_error, char_expr, LINE, Char, Ty})
    end;
do_type_check_expr_in(Env, Ty, Cons = {cons, LINE, H, T}) ->
    case expect_list_type(Ty, dont_allow_nil_type) of
	{elem_ty, ETy, Cs} ->
	    {VB1, Cs1} = type_check_expr_in(Env, ETy, H),
	    {VB2, Cs2} = type_check_expr_in(Env, Ty,  T),
	    {union_var_binds(VB1, VB2), constraints:combine([Cs, Cs1, Cs2])};
	{elem_tys, ETys, Cs} ->
	    {VB1, Cs1} = type_check_union_in(Env, ETys, H),
	    {VB2, Cs2} = type_check_expr_in(Env, Ty,  T),
	    {union_var_binds(VB1, VB2), constraints:combine([Cs, Cs1, Cs2])};
	any ->
	    {_Ty, VB1, Cs1} = type_check_expr   (Env, H),
	    {     VB2, Cs2} = type_check_expr_in(Env, Ty, T),
	    {union_var_binds(VB1, VB2), constraints:combine(Cs1, Cs2)};
	{type_error, _} ->
	    throw({type_error, cons, LINE, Cons, Ty})
    end;
do_type_check_expr_in(Env, Ty, {nil, LINE}) ->
    case subtype({type, LINE, nil, []}, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, nil, LINE, Ty})
    end;
do_type_check_expr_in(Env, Ty, {string, LINE, String}) ->
    case subtype({type, LINE, string, []}, Ty, Env#env.tenv) of
      {true, Cs} ->
        {#{}, Cs};
      false ->
        throw({type_error, string, LINE, String, Ty})
    end;
do_type_check_expr_in(Env, Ty, {bin, LINE, _BinElements} = Bin) ->
    %% Accept any binary type regardless of bit size parameters.
    %% TODO: If we can compute the length of the bit expression, we get the
    %%       exact type and can require that it's a subtype of Ty.
    Cs1 = case subtype(Ty, {type, LINE, binary, [{integer, LINE, 0},
						 {integer, LINE, 1}]},
		       Env#env.tenv) of
	      {true, Cs0} ->
		  Cs0;
	      false ->
		  throw({type_error, bin, LINE, Ty})
	  end,
    {_Ty, VarBinds, Cs2} = type_check_expr(Env, Bin),
    {VarBinds, constraints:combine(Cs1, Cs2)};
do_type_check_expr_in(Env, ResTy, {tuple, LINE, TS}) ->
    case expect_tuple_type(ResTy, length(TS)) of
	{elem_ty, Tys, Cs} ->
	    {VBs, Css} = lists:unzip([ type_check_expr_in(Env, Ty, Expr)
		                    || {Ty, Expr} <- lists:zip(Tys, TS) ]),
	    {union_var_binds(VBs), constraints:combine([Cs|Css])};
	{elem_tys, Tyss, Cs} ->
	    case type_check_tuple_union_in(Env, Tyss, TS) of
		none ->
		    throw({type_error, tuple, LINE, ResTy});
		{VBs, Css} ->
		    {union_var_binds(VBs), constraints:combine([Cs|Css])}
	    end;
	any ->
	    {_Tys, VBs, Css} = lists:unzip3([type_check_expr(Env, Expr)
					   || Expr <- TS ]),
	    {union_var_binds(VBs), constraints:combine(Css)};
	{type_error, _} ->
	    throw({type_error, tuple, LINE, ResTy})
    end;

%% Maps
do_type_check_expr_in(Env, ResTy, {map, LINE, Assocs}) ->
    case subtype(ResTy, {type, LINE, map, any}, Env#env.tenv) of
        {true, Cs1} ->
            %% TODO: check the type of the map fields
            {_AssocTy, VBs, Cs2} = type_check_assocs(Env, Assocs),
            {VBs, constraints:combine(Cs1, Cs2)};
        false ->
            throw({type_error, map, LINE, ResTy})
    end;
do_type_check_expr_in(Env, ResTy, {map, LINE, Expr, Assocs}) ->
    {Ty, VBExpr,   Cs1} = type_check_expr(Env, Expr),
    {_AssocTy, VBAssocs, Cs2} = type_check_assocs(Env, Assocs),
    % TODO: Update the type of the map.
    % TODO: Check the type of the map.
    case subtype(Ty, ResTy, Env#env.tenv) of
        {true, Cs3} ->
            {union_var_binds([VBExpr, VBAssocs]),
             constraints:combine([Cs1, Cs2, Cs3])};
        false ->
            throw({type_error, map, LINE, ResTy})
    end;

%% Records
do_type_check_expr_in(Env, ResTy, {record, P, Record, Fields}) ->
    Rec = maps:get(Record, Env#env.tenv#tenv.records),
    case expect_record_type(Record, ResTy, Env#env.tenv) of
      type_error ->
	    throw({type_error, record, P, Record, ResTy});
      {ok, Cs1} ->
	    {VarBinds, Cs2} = type_check_fields(Env, Rec, Fields),
	    {VarBinds, constraints:combine(Cs1, Cs2)}
    end;
do_type_check_expr_in(Env, ResTy, {record, P, Exp, Record, Fields}) ->
    RecordTy = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    case expect_record_type(Record, ResTy, Env#env.tenv) of
      type_error ->
        throw({type_error, record_update, P, Record, ResTy});
      {ok, Cs1} ->
	    Rec = maps:get(Record, Env#env.tenv#tenv.records),
	    {VarBindsList, Css}
		= lists:unzip(
		    lists:map(fun ({record_field, _, {atom, _, Field}, Expr}) ->
				      FieldTy = get_rec_field_type(Field, Rec),
				      type_check_expr_in(Env, FieldTy, Expr)
			      end
			     ,Fields)
		   ),
	    {VarBinds, Cs2} = type_check_expr_in(Env, RecordTy, Exp),
	    {union_var_binds([VarBinds|VarBindsList])
	    ,constraints:combine([Cs1, Cs2|Css])}
    end;
do_type_check_expr_in(Env, ResTy, {record_field, _, Expr, Record, {atom, _, Field}}) ->
    Rec = maps:get(Record, Env#env.tenv#tenv.records),
    FieldTy = get_rec_field_type(Field, Rec),
    case subtype(ResTy, FieldTy, Env#env.tenv) of
	{true, Cs1} ->
	    RecTy = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
	    {VarBinds, Cs2} = type_check_expr_in(Env, RecTy, Expr),
	    {VarBinds, constraints:combine([Cs1,Cs2])};
	false ->
	    %% TODO: Improve quality of error message
	    throw({type_error, record})
    end;
do_type_check_expr_in(Env, ResTy, {record_index, LINE, Record, Field}) ->
    case subtype(ResTy, {type, erl_anno:new(0), integer, []}, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, record_index, LINE, Record, Field})
    end;

do_type_check_expr_in(Env, ResTy, {'case', _, Expr, Clauses}) ->
    {ExprTy, VarBinds, Cs1} = type_check_expr(Env, Expr),
    Env2 = Env#env{ venv = add_var_binds(Env#env.venv, VarBinds) },
    {VB, Cs2} = check_clauses(Env2, [ExprTy], ResTy, Clauses),
    {VB, constraints:combine(Cs1,Cs2)};
do_type_check_expr_in(Env, ResTy, {'if', _, Clauses}) ->
    check_clauses(Env, ResTy, Clauses);
do_type_check_expr_in(Env, ResTy, {call, P, Name, Args}) ->
    {FunTy, VarBinds, Cs} = type_check_fun(Env, Name, length(Args)),
    {VarBinds2, Cs2} = type_check_call(Env, ResTy, expect_fun_type(FunTy), Args,
				       {P, Name, FunTy}),
    {union_var_binds(VarBinds, VarBinds2), constraints:combine(Cs, Cs2)};
do_type_check_expr_in(Env, ResTy, {'lc', P, Expr, Qualifiers}) ->
    type_check_lc_in(Env, ResTy, Expr, P, Qualifiers);

%% Functions
do_type_check_expr_in(Env, Ty, {'fun', P, {clauses, Clauses}}) ->
    case expect_fun_type(Ty) of
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
	    throw({type_error, lambda, P, Ty})
    end;
do_type_check_expr_in(Env, ResTy, {'fun', P, {function, Name, Arity}}) ->
    BoundedFunTypeList = get_type_from_name_arity(Name, Arity, Env#env.fenv, P),
    case any_subtype(ResTy, BoundedFunTypeList, Env#env.tenv) of
	{true, Cs} -> {#{}, Cs};
	false -> throw({type_error, fun_res_type, P, {atom, P, Name},
			ResTy, BoundedFunTypeList})
    end;
do_type_check_expr_in(Env, ResTy, {'fun', P, {function, M, F, A}}) ->
    case {get_atom(Env, M), get_atom(Env, F), A} of
	{{atom, _, Module}, {atom, _, Function}, {integer, _,Arity}} ->
	    case gradualizer_db:get_spec(Module, Function, Arity) of
		{ok, BoundedFunTypeList} ->
		    case any_subtype(ResTy, BoundedFunTypeList, Env#env.tenv) of
			{true, Cs} -> {#{}, Cs};
			false -> throw({type_error, mfa, P
				       ,Module, Function, Arity, ResTy
				       ,BoundedFunTypeList})
		    end;
		not_found ->
		    throw({call_undef, P, Module, Function, Arity})
	    end;
	_ -> % We don't have enough information to check the type.
	    {#{}, constraints:empty()}
    end;
do_type_check_expr_in(Env, Ty, {named_fun, P, FunName, Clauses}) ->
    NewEnv = Env#env{ venv = add_var_binds(#{ FunName => Ty }, Env#env.venv) },
    case expect_fun_type(Ty) of
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
	    throw({type_error, lambda, P, Ty})
    end;

do_type_check_expr_in(Env, ResTy, {'receive', _, Clauses}) ->
    check_clauses(Env, any, ResTy, Clauses);
do_type_check_expr_in(Env, ResTy, {'receive', _, Clauses, After, Block}) ->
    {VarBinds1, Cs1} = check_clauses(Env, any, ResTy, Clauses),
    {VarBinds2, Cs2} = type_check_expr_in(Env
                                         ,{type, erl_anno:new(0), integer, []}
                                         ,After),
    {VarBinds3, Cs3} = type_check_block_in(Env, ResTy, Block),
    {union_var_binds([VarBinds1, VarBinds2, VarBinds3])
                    ,constraints:combine([Cs1, Cs2, Cs3])};
do_type_check_expr_in(Env, ResTy, {op, _, '!', Arg1, Arg2}) ->
    % The first argument should be a pid.
    {_,  VarBinds1, Cs1} = type_check_expr(Env, Arg1),
    {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
    {union_var_binds([VarBinds1,VarBinds2]), constraints:combine(Cs1,Cs2)};
do_type_check_expr_in(Env, ResTy, {op, P, 'not', Arg}) ->
    case subtype(ResTy, {type, P, boolean, []}, Env#env.tenv) of
	{true, Cs1} ->
	    {VB, Cs2} = type_check_expr_in(Env, ResTy, Arg),
	    {VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, not_used_with_wrong_type, P, ResTy})
    end;
do_type_check_expr_in(Env, ResTy, {op, P, 'bnot', Arg}) ->
    case subtype(ResTy, {type, P, integer, []}, Env#env.tenv) of
	{true, Cs1} ->
	    {VB, Cs2} = type_check_expr_in(Env, ResTy, Arg),
	    {VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, bnot_used_with_wrong_type, P, ResTy})
    end;
do_type_check_expr_in(Env, ResTy, {op, P, '+', Arg}) ->
    case subtype(ResTy, {type, P, number, []}, Env#env.tenv) of
	{true, Cs1} ->
	    {VB, Cs2} = type_check_expr_in(Env, ResTy, Arg),
	    {VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, non_number_exp_type_plus, P, ResTy})
    end;
do_type_check_expr_in(Env, ResTy, {op, P, '-', Arg}) ->
    case subtype(ResTy, {type, P, number, []}, Env#env.tenv) of
	{true, Cs1} ->
	    {VB, Cs2} = type_check_expr_in(Env, ResTy, Arg),
	    {VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, non_number_exp_type_minus, P, ResTy})
    end;
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '+' orelse Op == '-' orelse Op == '*' orelse Op == '/' ->
    type_check_arith_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == 'div'  orelse Op == 'rem' orelse
      Op == 'band' orelse Op == 'bor' orelse Op == 'bxor' orelse
      Op == 'bsl'  orelse Op == 'bsr' ->
    type_check_int_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == 'and' orelse Op == 'or' orelse Op == 'xor' orelse
      Op == 'andalso' orelse Op == 'orelse' ->
    type_check_logic_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '=:=' orelse Op == '==' orelse
      Op == '=/=' orelse Op == '/=' orelse
      Op == '>=' orelse Op == '=<' orelse
      Op == '>' orelse Op == '<' ->
    type_check_rel_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '++' orelse Op == '--' ->
    type_check_list_op_in(Env, ResTy, Op, P, Arg1, Arg2);

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
    {_VB3, Cs3} = check_clauses(Env, any, ResTy, CatchCs),
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
    case subtype(ResTy, {type, erl_anno:new(0), 'number', []}, Env#env.tenv) of
	{true, Cs} ->
	  {VarBinds1, Cs1} = type_check_expr_in(Env, ResTy, Arg1),
	  {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
	  {union_var_binds([VarBinds1, VarBinds2])
	  ,constraints:combine([Cs, Cs1, Cs2])};
	false ->
	  throw({type_error, arith_error, Op, P, ResTy})
    end.
type_check_int_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    case subtype(ResTy, {type, erl_anno:new(0), 'integer', []}, Env#env.tenv) of
	{true, Cs} ->
	  {VarBinds1, Cs1} = type_check_expr_in(Env, ResTy, Arg1),
	  {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
	  {union_var_binds([VarBinds1, VarBinds2])
	  ,constraints:combine([Cs, Cs1, Cs2])};
	false ->
	  throw({type_error, int_error, Op, P, ResTy})
    end.
type_check_logic_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    case subtype(ResTy, {type, erl_anno:new(0), 'bool', []}, Env#env.tenv) of
	{true, Cs} ->
	  {VarBinds1, Cs1} = type_check_expr_in(Env, ResTy, Arg1),
	  EnvArg2 =
	      if Op =:= 'andalso'; Op =:= 'orelse' ->
	              %% variable bindings are propagated from Arg1 to Arg2
	              Env#env{venv = union_var_binds([Env#env.venv, VarBinds1])};
	         true ->
	              Env
	      end,
	  {VarBinds2, Cs2} = type_check_expr_in(EnvArg2, ResTy, Arg2),
	  {union_var_binds([VarBinds1, VarBinds2])
	  ,constraints:combine([Cs, Cs1, Cs2])};
	false ->
	  throw({type_error, logic_error, Op, P, ResTy})
    end.
type_check_rel_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    case subtype(ResTy, {type, erl_anno:new(0), 'bool', []}, Env#env.tenv) of
	{true, Cs0} ->
	  {ResTy1, VarBinds1, Cs1} = type_check_expr(Env, Arg1),
	  {ResTy2, VarBinds2, Cs2} = type_check_expr(Env, Arg2),
	  case compatible(ResTy1, ResTy2, Env#env.tenv) of
	      {true, Cs} ->
		  {union_var_binds([VarBinds1, VarBinds2])
		  ,constraints:combine([Cs0, Cs1, Cs2, Cs])};
	      false ->
		  throw({type_error, rel_error, Op, P, ResTy1, ResTy2})
	  end;
	false ->
	  throw({type_error, rel_error, Op, P, ResTy})
    end.
type_check_list_op_in(Env, ResTy, Op, P, Arg1, Arg2) ->
    case subtype(ResTy, {type, erl_anno:new(0), list, []}, Env#env.tenv) of
      {true, Cs} ->
        {VarBinds1, Cs1} = type_check_expr_in(Env, ResTy, Arg1),
	{VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
	  {union_var_binds([VarBinds1, VarBinds2])
	  ,constraints:combine([Cs, Cs1, Cs2])};
      false ->
	  throw({type_error, list_op_error, Op, P, ResTy})
    end.

type_check_lc_in(Env, ResTy, Expr, P, []) ->
    case expect_list_type(ResTy, allow_nil_type) of
	any ->
	    {_Ty, _VB, Cs} = type_check_expr(Env, Expr),
	    {#{}, Cs};
	{elem_ty, ElemTy, Cs1} ->
	    {_VB, Cs2} = type_check_expr_in(Env, ElemTy, Expr),
	    {#{}, constraints:combine(Cs1, Cs2)};
	{elem_tys, ElemTys, Cs1} ->
	    {VB, Cs2} = type_check_union_in(Env, ElemTys, Expr),
	    {VB, constraints:combine(Cs1, Cs2)};
	{type_error, Ty} ->
	    throw({type_error, lc, P, Ty})
    end;
type_check_lc_in(Env, ResTy, Expr, P, [{generate, P_Gen, Pat, Gen} | Quals]) ->
    {Ty, _VB1, Cs1} = type_check_expr(Env, Gen),
    case expect_list_type(Ty, allow_nil_type) of
	any ->
	    {_VB2, Cs2} = type_check_lc_in(Env#env{
					     venv =
						 add_any_types_pat(Pat
								  ,Env#env.venv)
					    }
					  ,ResTy, Expr, P, Quals),
	    {#{}, constraints:combine(Cs1, Cs2)};
	{elem_ty, ElemTy, Cs} ->
	    {NewEnv, Cs2} = add_type_pat(Pat
					,ElemTy
					,Env#env.tenv
					,Env#env.venv),
	    {_VB2, Cs3} = type_check_lc_in(Env#env{
					     venv = NewEnv
					    }
					  ,ResTy, Expr, P, Quals),
	    {#{}, constraints:combine([Cs, Cs1, Cs2, Cs3])};
	{elem_tys, _ElemTys, Cs} ->
	    %% TODO: As a hack, we treat a union type as any, just to
	    %% allow the program to type check.
	    {_VB2, Cs2} = type_check_lc_in(Env#env{
					     venv =
						 add_any_types_pat(Pat
								  ,Env#env.venv)
					    }
					  ,ResTy, Expr, P, Quals),
	    {#{}, constraints:combine([Cs, Cs1, Cs2])};
	{type_error, Ty} ->
	    throw({type_error, generator, P_Gen, Ty})
    end;
type_check_lc_in(Env, ResTy, Expr, P, [Pred | Quals]) ->
    %% We choose to check the type of the predicate here. Arguments can be
    %% made either way on whether we should check the type here.
    {_VB1, Cs1} = type_check_expr_in(Env, {type, erl_anno:new(0), 'boolean', []}, Pred),
    { VB2, Cs2} = type_check_lc_in(Env, ResTy, Expr, P, Quals),
    {VB2, constraints:combine(Cs1, Cs2)}.

type_check_assocs(Env, [{Assoc, _, Key, Val}| Assocs])
  when Assoc == map_field_assoc orelse Assoc == map_field_exact ->
    {_KeyTy, _KeyVB, Cs1} = type_check_expr(Env, Key),
    {_ValTy, _ValVB, Cs2} = type_check_expr(Env, Val),
    % TODO
    {Ty, VB, Cs} = type_check_assocs(Env, Assocs),
    {Ty, VB, constraints:combine([Cs, Cs1, Cs2])};
type_check_assocs(_Env, []) ->
    {{type, erl_anno:new(0), any, []}, #{}, constraints:empty()}.


type_check_fun(Env, {atom, P, Name}, Arity) ->
    % Local function call
    Types = get_type_from_name_arity(Name, Arity, Env#env.fenv, P),
    {Types, #{}, constraints:empty()};
type_check_fun(_Env, {remote, P, {atom,_,Module}, {atom,_,Fun}}, Arity) ->
    % Module:function call
    case gradualizer_db:get_spec(Module, Fun, Arity) of
	{ok, Types} -> {Types, #{}, constraints:empty()};
	not_found   -> throw({call_undef, P, Module, Fun, Arity})
    end;
type_check_fun(_Env, {remote, _, _Expr, _}, Arity)->
    % Call to an unknown module. Revert to dynamic types.
    {[{type, erl_anno:new(0), bounded_fun,
       [{type, erl_anno:new(0), 'fun',
	 [{type, erl_anno:new(0), product,
	   lists:duplicate(Arity, {type, erl_anno:new(0), any, []})},
	  {type,0,any,[]}]},
	[]]}], #{}, constraints:empty()};
type_check_fun(Env, Expr, _Arity) ->
    type_check_expr(Env, Expr).

type_check_call_intersection(Env, ResTy, [Ty], Args, E) ->
    type_check_call(Env, ResTy, Ty, Args, E);
type_check_call_intersection(Env, ResTy, Tys, Args, E) ->
    type_check_call_intersection_(Env, ResTy, Tys, Args, E).
type_check_call_intersection_(_Env, _ResTy, [], _Args, {P, Name, Ty}) ->
    throw({type_error, no_type_match_intersection, P, Name, Ty});
type_check_call_intersection_(Env, ResTy, [Ty | Tys], Args, E) ->
    try
        type_check_call(Env, ResTy, Ty, Args, E)
    catch
	Error when element(1, Error) == type_error ->
	    type_check_call_intersection_(Env, ResTy, Tys, Args, E)
    end.

type_check_call(Env, ResTy, {fun_ty, ArgsTy, FunResTy, Cs}, Args, {P, Name, _}) ->
    {VarBindsList, Css} =
	lists:unzip(
	  lists:zipwith(fun (ArgTy, Arg) ->
				type_check_expr_in(Env, ArgTy, Arg)
			end, ArgsTy, Args)
	 ),
    case subtype(FunResTy, ResTy, Env#env.tenv) of
	{true, Cs1} ->
	    { union_var_binds(VarBindsList)
	    , constraints:combine([Cs, Cs1 | Css]) };
	false ->
	    throw({type_error, fun_res_type, P, Name, FunResTy, ResTy})
    end;
type_check_call(Env, ResTy, {fun_ty_any_args, FunResTy, Cs}, Args, {P, Name, _})  ->
    {_Tys, VarBindsList, Css} =
	lists:unzip3(
	  lists:map(fun (Arg) ->
			    type_check_expr(Env, Arg)
		    end, Args)
	 ),
    case subtype(FunResTy, ResTy, Env#env.tenv) of
	{true, Cs1} ->
	    { union_var_binds(VarBindsList)
	    , constraints:combine([Cs, Cs1 | Css]) };
	false ->
	    throw({type_error, fun_res_type, P, Name, FunResTy, ResTy})
    end;
type_check_call(Env, _ResTy, any, Args, _E) ->
    {_Tys, VarBindsList, Css} =
	lists:unzip3(
	  lists:map(fun (Arg) ->
			    type_check_expr(Env, Arg)
		    end, Args)
	 ),
    {union_var_binds(VarBindsList), constraints:combine(Css)};
type_check_call(Env, ResTy, {fun_ty_intersection, Tys, Cs1}, Args, E) ->
    {VB, Cs2} = type_check_call_intersection(Env, ResTy, Tys, Args, E),
    {VB, constraints:combine(Cs1, Cs2)};
type_check_call(Env, ResTy, {fun_ty_union, Tys, Cs1}, Args, E) ->
    {VB, Cs2} = type_check_call_union(Env, ResTy, Tys, Args, E),
    {VB, constraints:combine(Cs1, Cs2)};
type_check_call(_Env, _ResTy, {type_error, _}, _Args, {P, Name, FunTy}) ->
    throw({type_error, expected_fun_type, P, Name, FunTy}).


type_check_call_union(_Env, _ResTy, [], _Args, _E) ->
    {#{}, constraints:empty()};
type_check_call_union(Env, ResTy, [Ty|Tys], Args, E) ->
    {VB1, Cs1} = type_check_call(Env, ResTy, Ty, Args, E),
    {VB2, Cs2} = type_check_call_union(Env, ResTy, Tys, Args, E),
    %% TODO: It's not clear to me what should be returned here.
    %% When combining all the varbinds we should really create
    %% a union of all types for a variable.
    {union_var_binds(VB1, VB2)
    ,constraints:combine(Cs1, Cs2)
    }.



type_check_block(Env, [Expr]) ->
    type_check_expr(Env, Expr);
type_check_block(Env, [Expr | Exprs]) ->
    {_, VarBinds, Cs1} = type_check_expr(Env, Expr),
    {Ty, VB, Cs2} = type_check_block(Env#env{ venv = add_var_binds(Env#env.venv, VarBinds) }, Exprs),
    {Ty, add_var_binds(VB, VarBinds), constraints:combine(Cs1, Cs2)}.

type_check_block_in(Env, ResTy, [Expr]) ->
    type_check_expr_in(Env, ResTy, Expr);
type_check_block_in(Env, ResTy, [Expr | Exprs]) ->
    {_, VarBinds, Cs1} = type_check_expr(Env, Expr),
    {VB, Cs2} = type_check_block_in(Env#env{ venv = add_var_binds(Env#env.venv, VarBinds) }, ResTy, Exprs),
    {add_var_binds(VB, VarBinds), constraints:combine(Cs1, Cs2)}.

type_check_union_in(Env, [Ty|Tys], Expr) ->
    try
	type_check_expr_in(Env, Ty, Expr)
    catch
	E when element(1,E) == type_error ->
	    type_check_union_in(Env, Tys, Expr)
    end;
type_check_union_in(_Env, [], _Expr) ->
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
    {union_var_binds([VB1, VB2]), constraints:combine(Cs1, Cs2)};
type_check_cons_in(Env, Ty = {type, _, List, [ElemTy]}, H, T)
    when List == list orelse List == nonempty_list ->
    {VB1, Cs1} = type_check_expr_in(Env, ElemTy, H),
    {VB2, Cs2} = type_check_expr_in(Env, Ty,     T),
    {union_var_binds([VB1, VB2]), constraints:combine(Cs1, Cs2)};
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


get_type_from_name_arity(Name, Arity, FEnv, P) ->
    case maps:find({Name, Arity}, FEnv) of
	{ok, Types} ->
	    Types;
	error ->
	    case erl_internal:bif(Name, Arity) of
		true ->
		    {ok, Types} = gradualizer_db:get_spec(erlang, Name, Arity),
		    Types;
		false ->
		    throw({call_undef, P, Name, Arity})
	    end
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


%% We don't use these function right now but they can be useful for
%% implementing an approximation when typechecking unions of tuples.
split_tuple_type(N, {type, P, tuple, any}) ->
    [lists:duplicate(N, {type, P, any, []})];
split_tuple_type(_N, {type, _, tuple, Tys}) ->
    [Tys];
split_tuple_type(N, {type, _, union, Tys}) ->
    split_tuple_union(N, Tys).

split_tuple_union(N, [Tuple = {type, _, tuple, _}|Tys]) ->
    split_tuple_type(N, Tuple) ++ split_tuple_union(N, Tys);
split_tuple_union(_, []) ->
    [];
split_tuple_union(N, [{type, _, union, Tys1} | Tys2]) ->
    split_tuple_union(N, Tys1 ++ Tys2).


%% Infers (or at least propagates types from) fun, receive and try clauses.
infer_clauses(Env, Clauses) ->
    {Tys, VarBindsList, Css} =
	lists:unzip3(lists:map(fun (Clause) ->
				       infer_clause(Env, Clause)
			       end, Clauses)),
    {normalize({type, erl_anno:new(0), union, Tys}, Env#env.tenv)
    ,union_var_binds(VarBindsList)
    ,constraints:combine(Css)}.

infer_clause(Env, {clause, _, Args, Guards, Block}) ->
    EnvNew = Env#env{ venv = add_any_types_pats(Args, Env#env.venv) },
    % TODO: Can there be variable bindings in a guard? Right now we just
    % discard them.
    % TODO: Should we check that guards return boolean()?
    lists:map(fun (GuardConj) ->
		      lists:map(fun (Guard) ->
					type_check_expr(EnvNew, Guard)
				end, GuardConj)
	      end, Guards),
    type_check_block(EnvNew, Block).


check_clauses_intersect(Env, Ty, Clauses) when not is_list(Ty) ->
    check_clauses_fun(Env, Ty, Clauses);
check_clauses_intersect(_Env, [], _Clauses) ->
    {#{}, constraints:empty()};
check_clauses_intersect(Env, [Ty|Tys], Clauses) ->
    {VarBinds1, Cs1} = check_clauses_fun(Env, Ty, Clauses),
    {VarBinds2, Cs2} = check_clauses_intersect(Env, Tys, Clauses),
    {union_var_binds(VarBinds1, VarBinds2), constraints:combine(Cs1, Cs2)}.

check_clauses_union(_Env, [], _Clauses) ->
    %% TODO: Improve quality of type error
    throw({typer_error, check_clauses});
check_clauses_union(Env, [Ty|Tys], Clauses) ->
    try
	check_clauses_fun(Env, Ty, Clauses)
    catch
	Error when element(1,Error) == type_error ->
	    check_clauses(env, Tys, Clauses)
    end.


check_clauses_fun(Env, {fun_ty, ArgsTy, FunResTy, Cs1}, Clauses) ->
    {VarBinds, Cs2} = check_clauses(Env, ArgsTy, FunResTy, Clauses),
    {VarBinds, constraints:combine(Cs1, Cs2)};
check_clauses_fun(Env, {fun_ty_any_args, FunResTy, Cs1}, Clauses) ->
    {VarBinds, Cs2} = check_clauses(Env, any, FunResTy, Clauses),
    {VarBinds, constraints:combine(Cs1, Cs2)};
check_clauses_fun(Env, any, Clauses) ->
    check_clauses(Env, any, {type, erl_anno:new(0), any, []}, Clauses);
check_clauses_fun(Env, {fun_ty_intersection, Tys, Cs1}, Clauses) ->
    {VarBinds, Cs2} = check_clauses_intersect(Env, Tys, Clauses),
    {VarBinds, constraints:combine(Cs1, Cs2)};
check_clauses_fun(Env, {fun_ty_union, Tys, Cs1}, Clauses) ->
    {VarBinds, Cs2} = check_clauses_union(Env, Tys, Clauses),
    {VarBinds, constraints:combine(Cs1, Cs2)}.


check_clauses(Env, {var, _, TyVar}, Clauses) ->
    {Ty, VarBinds, Cs} = infer_clauses(Env, Clauses),
    {VarBinds, constraints:combine(constraints:upper(TyVar, Ty), Cs)};
check_clauses(Env, Ty, Clauses) ->
    {VarBindsList, Css} =
	lists:unzip(lists:map(fun (Clause) ->
				      check_clause(Env, Ty, Clause)
			      end, Clauses)),
    {union_var_binds(VarBindsList), constraints:combine(Css)}.

check_clauses(Env, ArgsTy, ResTy, Clauses) when
      not is_list(ArgsTy) andalso ArgsTy /= any ->
    check_clauses(Env, [ArgsTy], ResTy, Clauses);
check_clauses(Env, ArgsTy, ResTy, Clauses) ->
    {VarBindsList, Css} =
	lists:unzip(lists:map(fun (Clause) ->
				  check_clause(Env, ArgsTy, ResTy, Clause)
			  end, Clauses)),
    {union_var_binds(VarBindsList), constraints:combine(Css)}.

%% This version of check_clause is for "function-like" clauses, which
%% takes arguments. That includes case clauses and function clauses
%% but not if clauses.
check_clause(Env, any,    ResTy, {clause, _, Args,      _,     _} = Clause) ->
    ArgsTy = lists:duplicate(length(Args), {type, erl_anno:new(0), any, []}),
    check_clause(Env, ArgsTy, ResTy, Clause);
check_clause(Env, ArgsTy, ResTy, {clause, P, Args, Guards, Block}) ->
    case {length(ArgsTy), length(Args)} of
	{L, L} ->
	    {Env2, Cs1} = add_types_pats(Args, ArgsTy,
					 Env#env.tenv, Env#env.venv),
	    EnvNew      = Env#env{ venv =  Env2 },
	    VarBinds1   = check_guards(EnvNew, Guards),
	    EnvNewest   = EnvNew#env{ venv = add_var_binds(EnvNew#env.venv, VarBinds1) },
	    {VarBinds2, Cs2} = type_check_block_in(EnvNewest, ResTy, Block),
	    {union_var_binds(VarBinds1, VarBinds2)
	    ,constraints:combine(Cs1, Cs2)};
        {LenTy, LenArgs} ->
	    throw({argument_length_mismatch, P, LenTy, LenArgs})
    end;
%% DEBUG
check_clause(_Env, _ArgsTy, _ResTy, Term) ->
    io:format("DEBUG: check_clause term: ~p~n", [Term]),
    throw(check_clause).

%% This version, with only three arguments, is for if clauses. Those
%% clauses don't take any arguments.
check_clause(Env, ResTy, {clause, _, [], Guards, Block}) ->
    VarBinds1 = check_guards(Env, Guards),
    NewEnv    = Env#env{ venv = add_var_binds(Env#env.venv, VarBinds1) },
    {VarBinds2, Cs2} = type_check_block_in(NewEnv, ResTy, Block),
    {union_var_binds(VarBinds1, VarBinds2), Cs2}.


%% TODO: implement proper checking of guards.
check_guards(Env, Guards) ->
    union_var_binds(
      lists:map(fun (GuardSeq) ->
			union_var_binds(
			  lists:map(fun (Guard) ->
						{_Ty, VB, _Cs} = type_check_expr(Env, Guard), % Do we need to thread the Env?
						VB
				    end, GuardSeq))
		end, Guards)).

type_check_function(Env, {function,_, Name, NArgs, Clauses}) ->
    case maps:find({Name, NArgs}, Env#env.fenv) of
	{ok, FunTy} ->
	    check_clauses_fun(Env, expect_fun_type(FunTy), Clauses);
	error ->
	    throw({internal_error, missing_type_spec, Name, NArgs})
    end.

add_types_pats([], [], _TEnv, VEnv) ->
    ret(VEnv);
add_types_pats([Pat | Pats], [Ty | Tys], TEnv, VEnv) ->
    NormTy = normalize(Ty, TEnv),
    {VEnv2, Cs1} = add_type_pat(Pat, NormTy, TEnv, VEnv),
    {VEnv3, Cs2} = ?throw_orig_type(add_types_pats(Pats, Tys, TEnv, VEnv2),
				    Ty, NormTy),
    {VEnv3, constraints:combine(Cs1, Cs2)}.

add_type_pat({var, _, '_'}, _Ty, _TEnv, VEnv) ->
    ret(VEnv);
add_type_pat({var, _, A}, Ty, _TEnv, VEnv) ->
    ret(VEnv#{ A => Ty });
add_type_pat(Expr, {type, _, any, []}, _TEnv, VEnv) ->
    ret(add_any_types_pat(Expr, VEnv));
add_type_pat(Lit = {integer, P, _}, Ty, TEnv, VEnv) ->
    case subtype(Lit, Ty, TEnv) of
      {true, Cs} ->
         {VEnv, Cs};
     false ->
         throw({type_error, pattern, P, Lit, Ty})
    end;
add_type_pat(Lit = {char, P, Val}, Ty, TEnv, VEnv) ->
    case subtype({integer, P, Val}, Ty, TEnv) of
      {true, Cs} ->
	    {VEnv, Cs};
	false ->
	    throw({type_error, pattern, P, Lit, Ty})
    end;
add_type_pat(Lit = {float, P, _}, Ty, TEnv, VEnv) ->
    case subtype({type, erl_anno:new(0), float, []}, Ty, TEnv) of
      {true, Cs} ->
	    {VEnv, Cs};
	false ->
	    throw({type_error, pattern, P, Lit, Ty})
    end;
add_type_pat(Tuple = {tuple, P, Pats}, Ty, TEnv, VEnv) ->
    case expect_tuple_type(normalize(Ty, TEnv), length(Pats)) of
	any ->
	    {union_var_binds([add_any_types_pat(Pat, VEnv) || Pat <- Pats])
	    ,constraints:empty()};
	{elem_ty, Tys, Cs} ->
	    lists:foldl(fun ({Pat, TTy}, {Env1, Cs1}) ->
				{Env2, Cs2} = add_type_pat(Pat, TTy, TEnv, Env1),
				{Env2, constraints:combine(Cs1, Cs2)}
			end
		       ,{VEnv, Cs}
		       ,lists:zip(Pats, Tys));
	{elem_tys, Tyss, Cs} ->
	    %% TODO: This code approximates unions of tuples with tuples of unions
	    Unions = lists:map(fun (UnionTys) ->
				       {type, erl_anno:new(0), union, UnionTys}
			       end
			      ,transpose(Tyss)),
	    lists:foldl(fun ({Pat, Union}, {Env1, Cs1}) ->
				{Env2, Cs2} = add_type_pat(Pat, Union, TEnv, Env1),
				{Env2, constraints:combine(Cs1, Cs2)}
			end
		       ,{VEnv, Cs}
		       ,lists:zip(Pats, Unions));
	{type_error, _Type} ->
	    throw({type_error, pattern, P, Tuple, Ty})
    end;
add_type_pat(Atom = {atom, P, _}, Ty, TEnv, VEnv) ->
    case subtype(Atom, Ty, TEnv) of
	{true, Cs} ->
	    {VEnv, Cs};
	false ->
	    throw({type_error, pattern, P, Atom, Ty})
    end;
add_type_pat(Nil = {nil, P}, Ty, TEnv, VEnv) ->
    case subtype({type, P, nil, []}, Ty, TEnv) of
	{true, Cs} ->
	    {VEnv, Cs};
	false ->
	    throw({type_error, pattern, P, Nil, Ty})
    end;
add_type_pat(CONS = {cons, P, PH, PT}, ListTy, TEnv, VEnv) ->
    case expect_list_type(normalize(ListTy, TEnv), dont_allow_nil_type) of
	any ->
            VEnv2 = add_any_types_pat(PH, VEnv),
            add_type_pat(PT, ListTy, TEnv, VEnv2);
	{elem_ty, ElemTy, Cs1} ->
	    {VEnv2, Cs2} = add_type_pat(PH, ElemTy, TEnv, VEnv),
            {VEnv3, Cs3} = add_type_pat(PT, ListTy, TEnv, VEnv2),
	    {VEnv3, constraints:combine([Cs1, Cs2, Cs3])};
	{elem_tys, _ElemTys, Cs1} ->
	    %% TODO: As a hack, we treat a union type as any, just to
	    %% allow the program to type check.
            VEnv2 = add_any_types_pat(PH, VEnv),
            {VEnv3, Cs2} = add_type_pat(PT, ListTy, TEnv, VEnv2),
	    {VEnv3, constraints:combine(Cs1, Cs2)};
	{type_error, _Ty} ->
	    throw({type_error, cons_pat, P, CONS, ListTy})
    end;
add_type_pat(String = {string, P, _}, Ty, _TEnv, VEnv) ->
   case subtype({type, P, string, []}, Ty, VEnv) of
     {true, Cs} ->
       {VEnv, Cs};
     false ->
       throw({type_error, pattern, P, String, Ty})
   end;
add_type_pat({bin, _, BinElements}, {type, _, binary, [_,_]}, TEnv, VEnv) ->
    %% TODO: Consider the bit size parameters
    lists:foldl(fun ({bin_element, _, Pat, _Size, Specifiers}, {VEnv1, Cs1}) ->
			    %% Check Pat against the bit syntax type specifiers
			    SpecTy = bit_specifier_list_to_type(Specifiers),
			    {VEnv2, Cs2} = add_type_pat(Pat, SpecTy, TEnv, VEnv1),
			    {VEnv2, constraints:combine(Cs1, Cs2)}
		    end,
		    {VEnv, constraints:empty()},
		    BinElements);
add_type_pat({record, P, Record, Fields}, Ty, TEnv, VEnv) ->
    case expect_record_type(Record, Ty, TEnv) of
        type_error -> throw({type_error, record_pattern, P, Record, Ty});
        {ok, Cs1} ->
	    {VEnv2, Cs2} = add_type_pat_fields(Fields, Record, TEnv, VEnv),
	    {VEnv2, constraints:combine(Cs1, Cs2)}
    end;
add_type_pat({match, _, Pat1, Pat2}, Ty, TEnv, VEnv) ->
    {VEnv1, Cs1} = add_type_pat(Pat2, Ty, TEnv, VEnv),
    {VEnv2, Cs2} = add_type_pat(Pat1, Ty, TEnv, VEnv1),
    {VEnv2, constraints:combine(Cs1, Cs2)};
add_type_pat({op, _, '++', Pat1, Pat2}, Ty, TEnv, VEnv) ->
    {VEnv1, Cs1} = add_type_pat(Pat1, Ty, TEnv, VEnv),
    {VEnv2, Cs2} = add_type_pat(Pat2, Ty, TEnv, VEnv1),
    {VEnv2, constraints:combine(Cs1,Cs2)};
add_type_pat(Expr={op, P, _Op, _Pat1, _Pat2}, Ty, TEnv, VEnv) ->
    %% Operator patterns are evaluated at compile-time by the compiler.
    %% So we simply evaluate them and check the type of the resulting value.
    %% This simplified situations like this: suppose Ty = non_neg_integer() and
    %% the pattern is 12 - 13. Is this pattern of the correct type? It is not
    %% enough to just check the two arguments to the operator (which would say that
    %% the pattern has the correct type) but we also need to check that the left
    %% argument is not smaller than the right argument. But instead of implementing
    %% such a check, we simply evaluate the pattern as an expression.
    {value, Val, _} = erl_eval:expr(Expr, orddict:new()),
    ValType = if is_integer(Val) -> {integer, erl_anno:new(0), Val};
                 is_float(Val)   -> {type,    erl_anno:new(0), float, []}
              end,
    case subtype(ValType, Ty, TEnv) of
      {true, Cs} ->
        {VEnv, Cs};
      false ->
        throw({type_error, operator_pattern, P, Expr, Ty})
    end;
add_type_pat(Expr={op, P, _Op, _Pat}, Ty, TEnv, VEnv) ->
    {value, Val, _} = erl_eval:expr(Expr, orddict:new()),
    ValType = if is_integer(Val) -> {integer, erl_anno:new(0), Val};
                 is_float(Val)   -> {type,    erl_anno:new(0), float, []}
              end,
    case subtype(ValType, Ty, TEnv) of
      {true, Cs} ->
        {VEnv, Cs};
      false ->
        throw({type_error, operator_pattern, P, Expr, Ty})
    end;
add_type_pat(Pat, {ann_type, _, [_, Ty]}, TEnv, VEnv) ->
    add_type_pat(Pat, Ty, TEnv, VEnv);

add_type_pat(Pat, Ty, _TEnv, _VEnv) ->
    throw({type_error, pattern, element(2, Pat), Pat, Ty}).

add_type_pat_fields([], _, _TEnv, VEnv) ->
    ret(VEnv);
add_type_pat_fields([{record_field, _, {atom, _, Field}, Pat}|Fields], Record, TEnv, VEnv) ->
    Rec = maps:get(Record, TEnv#tenv.records),
    FieldTy = get_rec_field_type(Field, Rec),
    {VEnv2, Cs1} = add_type_pat(Pat, FieldTy, TEnv, VEnv),
    {VEnv3, Cs2} = add_type_pat_fields(Fields, Record, TEnv, VEnv2),
    {VEnv3, constraints:combine(Cs1, Cs2)}.



add_type_pat_list([Pat|Pats], [Ty|Tys], TEnv, VEnv) ->
    {VEnv2, Cs1} = add_type_pat(Pat, Ty, TEnv, VEnv),
    {VEnv3, Cs2} = add_type_pat_list(Pats, Tys, TEnv, VEnv2),
    {VEnv3, constraints:combine(Cs1, Cs2)};
add_type_pat_list([], [], _TEnv, VEnv) ->
    ret(VEnv).

add_type_pat_tuple(Pats, {type, _, any, []}, _TEnv, VEnv) ->
    ret(add_any_types_pats(Pats, VEnv));
add_type_pat_tuple(Pats, {type, _, tuple, any}, _TEnv, VEnv) ->
    ret(add_any_types_pats(Pats, VEnv));
add_type_pat_tuple(Pats, {type, _, tuple, Tys}, TEnv, VEnv) ->
    add_types_pats(Pats, Tys, TEnv, VEnv);
add_type_pat_tuple(Pats, {type, _, union, Tys}, TEnv, VEnv) ->
%% TODO: This code approximates unions of tuples with tuples of unions
    Unions =
	lists:map(fun (UnionTys) ->
			  {type, erl_anno:new(0), union, UnionTys}
		  end
		 ,transpose([TS
			   || {type, _, tuple, TS} <- Tys
			    , length(TS) == length(Pats)])),
    lists:foldl(fun ({Pat, Union}, {Env1, Cs1}) ->
			{Env2, Cs2} = add_type_pat(Pat, Union, TEnv, Env1),
			{Env2, constraints:combine(Cs1, Cs2)}
		end, {VEnv, constraints:empty()}, lists:zip(Pats, Unions));
%add_type_pat_tuple(Pats, {var, _, Var},
add_type_pat_tuple(Pats, {ann_type, _, [_, Ty]}, TEnv, VEnv) ->
    add_type_pat_tuple(Pats, Ty, TEnv, VEnv).


transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].


add_any_types_pats([], VEnv) ->
    VEnv;
add_any_types_pats([Pat|Pats], VEnv) ->
    add_any_types_pats(Pats, add_any_types_pat(Pat, VEnv)).

add_any_types_pat(A, VEnv) when is_atom(A) -> % Is this case needed?
    VEnv;
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
add_any_types_pat({var, _,'_'}, VEnv) ->
    VEnv;
add_any_types_pat({var, _,A}, VEnv) ->
    VEnv#{ A => {type, erl_anno:new(0), any, []} };
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
-spec bit_specifier_list_to_type([atom()] | default) -> type().
bit_specifier_list_to_type(default) ->
    bit_specifier_list_to_type([integer]);
bit_specifier_list_to_type(Specifiers) ->
    TypeSpecifiers =
	lists:filtermap(fun
			    (S) when S == integer; S == utf8; S == utf16 ->
				{true, {type, erl_anno:new(0), integer, []}};
			    (float) ->
				{true, {type, 0, float, []}};
			    (S) when S == binary; S == bytes ->
				{true, {type, erl_anno:new(0), binary,
					[{integer, erl_anno:new(0), 0}
					,{integer, erl_anno:new(0), 8}]}};
			    (S) when S == bitstring; S == bits ->
				{true, {type, erl_anno:new(0), binary,
					[{integer, erl_anno:new(0), 0}
					,{integer, erl_anno:new(0), 1}]}};
			    (_NotATypeSpecifier) ->
				false
			end,
			Specifiers),
    case TypeSpecifiers of
	[]  -> {type, erl_anno:new(0), integer, []}; %% default
	[T] -> T
    end.

%%% Helper functions

return(X) ->
    { X, #{}, constraints:empty() }.

union_var_binds(VB1, VB2) ->
    union_var_binds([VB1, VB2]).

union_var_binds([]) ->
    #{};
union_var_binds([ VarBinds | VarBindsList ]) ->
    gradualizer_lib:merge_with(fun glb_types/3, VarBinds, union_var_binds(VarBindsList)).

add_var_binds(VEnv, VarBinds) ->
    gradualizer_lib:merge_with(fun glb_types/3, VEnv, VarBinds).

% TODO: improve
% Is this the right function to use or should I always just return any()?
glb_types(K, {type, _, N, Args1}, {type, _, N, Args2})
  when is_list(Args1), is_list(Args2) ->
    Args = [ glb_types(K, Arg1, Arg2) || {Arg1, Arg2} <- lists:zip(Args1, Args2) ],
    {type, erl_anno:new(0), N, Args};
glb_types(_, {type, _, N, any}, {type, _, N, _} = Ty2)
  when N =:= tuple; N =:= map ->
    Ty2;
glb_types(_, {type, _, N, _} = Ty1, {type, _, N, any})
  when N =:= tuple; N =:= map ->
    Ty1;
glb_types(_, _, _) ->
    {type, erl_anno:new(0), any, []}.

get_rec_field_type(FieldName,
                   [{typed_record_field,
                     {record_field, _,
                      {atom, _, FieldName}, _}, Ty}|_]) ->
    Ty;
get_rec_field_type(FieldName, [_|RecFieldTypes]) ->
    get_rec_field_type(FieldName, RecFieldTypes);
get_rec_field_type(FieldName, []) ->
    throw({error, {record_field_not_found, FieldName}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_check_forms(Forms, Opts) ->
    StopOnFirstError = proplists:get_bool(stop_on_first_error, Opts),
    File = proplists:get_value(print_file, Opts),

    case gradualizer_db:start_link() of
	{ok, _Pid}                    -> ok;
	{error, {already_started, _}} -> ok
    end,
    ParseData =
	collect_specs_types_opaques_and_functions(Forms),
    Env = create_env(ParseData, Opts),
    lists:foldr(fun (Function, Res) when Res =:= ok;
                                         not StopOnFirstError ->
			try type_check_function(Env, Function) of
			    {_VarBinds, _Cs} ->
				Res
			catch
			    Throw ->
				% Useful for debugging
				% io:format("~p~n", [erlang:get_stacktrace()]),
                                case File of
                                    undefined -> ok;
                                    _ -> io:format("~s: ", [File])
                                end,
				handle_type_error(Throw),
				nok;
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
		    (_Function, Err) ->
			Err
		end, ok, ParseData#parsedata.functions).

create_env(#parsedata{specs     = Specs
                     ,functions = Funs
                     ,types     = Types
                     ,opaques   = Opaques
                     ,records   = Records
                     }, Opts) ->
    FEnv = create_fenv(Specs, Funs),
    TEnv = create_tenv(Types ++ Opaques, Records),
    #env{fenv = FEnv,
         tenv = TEnv,
         %% Store some type checking options in the environment
         infer = proplists:get_bool(infer, Opts)}.

create_tenv(TypeDefs, RecordDefs) ->
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
    #tenv{types   = TypeMap,
          records = RecordMap}.

create_fenv(Specs, Funs) ->
% We're taking advantage of the fact that if a key occurrs more than once
% in the list then it right-most occurrence will take precedence. In this
% case it will mean that if there is a spec, then that will take precedence
% over the default type any().
    maps:from_list(
      % Built-in macro. TODO: Improve the type. We could provide a very
      % exact type for record_info by evaluating the call at compile time.
      [ {{record_info, 2}, {type, erl_anno:new(0), any, []}}
      ] ++
      [ {{Name, NArgs}, {type, erl_anno:new(0), any, []}}
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

handle_type_error({call_undef, LINE, Func, Arity}) ->
    io:format("Call to undefined function ~p/~p on line ~p~n",
              [Func, Arity, LINE]);
handle_type_error({call_undef, LINE, Module, Func, Arity}) ->
    io:format("Call to undefined function ~p:~p/~p on line ~p~n",
              [Module, Func, Arity, LINE]);
handle_type_error({undef, record, {{atom, LINE, Module}, {atom, _, RecName}}}) ->
    io:format("Undefined record ~p:~p on line ~p~n",
              [Module, RecName, LINE]);
handle_type_error({undef, record, {atom, LINE, RecName}}) ->
    io:format("Undefined record ~p on line ~p~n",
              [RecName, LINE]);
handle_type_error({undef, Type, {{atom, LINE, Module}, {atom, _, Name}, Arity}})
  when Type =:= user_type; Type =:= remote_type ->
    io:format("Undefined ~p ~p:~p/~p on line ~p~n",
              [Type, Module, Name, Arity, LINE]);
handle_type_error({undef, user_type, {{atom, LINE, Name}, Arity}}) ->
    io:format("Undefined user type ~p/~p on line ~p~n",
              [Name, Arity, LINE]);
handle_type_error({not_exported, remote_type, {{atom, LINE, _} = Module, Name, Arity}}) ->
    io:format("The type ~s:~s/~p on line ~p is not exported~n",
              [erl_pp:expr(Module), erl_pp:expr(Name), Arity, LINE]);
handle_type_error({type_error, tyVar, LINE, Var, VarTy, Ty}) ->
    io:format("The variable ~p on line ~p has type ~s "
	      "but is expected to have type ~s~n",
	      [Var, LINE, typelib:pp_type(VarTy), typelib:pp_type(Ty)]);
handle_type_error({type_error, char_expr, LINE, Char, Ty}) ->
    io:format("The character ~p on line ~p does not have type ~s~n"
             ,[erl_pp:expr(Char), LINE, typelib:pp_type(Ty)]);
handle_type_error({type_error, {atom, _, A}, LINE, Ty}) ->
    io:format("The atom ~p on line ~p does not have type ~s~n",
	      [A, LINE, typelib:pp_type(Ty)]);
handle_type_error({type_error, string, LINE, String, Ty}) ->
    io:format("The string ~p on line ~p does not have type ~s~n",
              [String, LINE, typelib:pp_type(Ty)]);
handle_type_error({type_error, int, I, LINE, Ty}) ->
    io:format("The integer ~p on line ~p does not have type ~s~n",
	      [I, LINE, typelib:pp_type(Ty)]);
handle_type_error({type_error, float, F, LINE, Ty}) ->
    io:format("The float ~p on line ~p does not have type ~s~n",
	      [F, LINE, typelib:pp_type(Ty)]);
handle_type_error({type_error, compat, _LINE, Ty1, Ty2}) ->
    io:format("The type ~s is not compatible with type ~s~n"
	     ,[typelib:pp_type(Ty1), typelib:pp_type(Ty2)]);
handle_type_error({type_error, list, _, Ty1, Ty}) ->
    io:format("The type ~s cannot be an element of a list of type ~s~n",
	      [typelib:pp_type(Ty1), typelib:pp_type(Ty)]);
handle_type_error({type_error, list, _, Ty}) ->
    io:format("The type ~s on line ~p is not a list type~n",
	      [typelib:pp_type(Ty), line_no(Ty)]);
handle_type_error({type_error, cons_pat, P, Cons, Ty}) ->
    io:format("The pattern ~s on line ~p does not have type:~n~s~n"
             ,[erl_pp:expr(Cons),P, typelib:pp_type(Ty)]);
handle_type_error({type_error, cons, P, Cons, Ty}) ->
    io:format("The expression ~s on line ~p does not have type ~s~n"
             ,[erl_pp:expr(Cons), P, typelib:pp_type(Ty)]);
handle_type_error({type_error, nil, LINE, Ty}) ->
    io:format("The empty list on line ~p does not have type ~s~n",
	      [LINE, typelib:pp_type(Ty)]);
handle_type_error({argument_length_mismatch, P, LenTy, LenArgs}) ->
    io:format("The clause on line ~p is expected to have ~p argument(s) "
              "but it has ~p~n ",
	      [P, LenTy, LenArgs]);
handle_type_error({type_error, call, _P, Name, TyArgs, ArgTys}) ->
    io:format("The function ~p expects arguments of type~n~p~n but is given "
	      "arguments of type~n~p~n",
	      [Name, TyArgs, ArgTys]);
handle_type_error({type_error, call, P, FunTy, Name}) ->
    io:format("The function ~s, called on line ~p doesn't have a function type~n"
             "Rather, it has the following type~n~s~n"
            ,[erl_pp:expr(Name), P, pp_intersection_type(FunTy)]);
handle_type_error({type_error, call_intersect, P, FunTy, Name}) ->
    io:format("The type of the function ~s, called on line ~p doesn't match "
              "the surrounding calling context.~n"
              "It has the following type~n~s~n"
             ,[erl_pp:expr(Name), P, pp_intersection_type(FunTy)]);
handle_type_error({type_error, mfa, P, M, F, A, ResTy, FunTy}) ->
    io:format("The mfa ~p:~p/~p on line ~p is expected to have type : ~n~s~n"
              "but has type : ~n"
              "~s~n"
             ,[M, F, A, P,typelib:pp_type(ResTy)
              ,pp_intersection_type(FunTy)]);
handle_type_error({type_error, fun_res_type, P, Func, FunResTy, ResTy}) ->
    Name = erl_pp:expr(Func), %% {atom, _, Name} or {remote, Mod, Name}
    io:format("The function ~s on line ~p is expected to return ~s but it returns ~s~n",
              [Name, P, typelib:pp_type(ResTy), typelib:pp_type(FunResTy)]);
handle_type_error({type_error, expected_fun_type, P, Func, FunTy}) ->
    Name = erl_pp:expr(Func),
    io:format("Expected function ~s on line ~p to have a function type,~n"
	      "but it has the following type:~n~s~n",
	      [Name, P, typelib:pp_type(FunTy)]);
handle_type_error({type_error, no_type_match_intersection, P, Func, FunTy}) ->
    Name = erl_pp:expr(Func),
    io:format("None of the types of the function ~s at line ~p matches the "
	      "call site. Here's the types of the function:~n~s~n",
	      [Name, P, pp_intersection_type(FunTy)]);
handle_type_error({type_error, boolop, BoolOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-boolean argument "
	      "of type ~s~n", [BoolOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, relop, RelOp, P, Ty1, Ty2}) ->
    io:format("The operator ~p on line ~p requires arguments of "
	      "compatible types.~nHowever, it has arguments "
	      "of type ~s and ~s~n", [RelOp, P, typelib:pp_type(Ty1)
				              , typelib:pp_type(Ty2)]);
handle_type_error({type_error, arith_error, ArithOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-numeric argument "
	      "of type ~s~n", [ArithOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, int_error, IntOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-integer argument "
	      "of type ~s~n", [IntOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, non_number_exp_type_plus, P, Ty}) ->
    io:format("The plus expression on line ~p is expected to have a "
	      "non-numeric type:~n~s~n", [P, typelib:pp_type(Ty)]);
handle_type_error({type_error, non_number_argument_to_plus, P, Ty}) ->
    io:format("The plus expression on line ~p has a non-numeric argument "
	      "of type:~n~s~n", [P, typelib:pp_type(Ty)]);
handle_type_error({type_error, non_number_exp_type_minus, P, Ty}) ->
    io:format("The negated expression on line ~p is expected to have a "
	      "non-numeric type:~n~s~n", [P, typelib:pp_type(Ty)]);
handle_type_error({type_error, non_number_argument_to_minus, P, Ty}) ->
    io:format("The negated expression on line ~p has a non-numeric argument "
	      "of type:~n~s~n", [P, typelib:pp_type(Ty)]);
handle_type_error({type_error, non_boolean_argument_to_not, P, Ty}) ->
    io:format("The 'not' expression on line ~p has a non-boolean argument "
	      "of type ~s~n", [P, typelib:pp_type(Ty)]);
handle_type_error({type_error, not_used_with_wrong_type, P, Ty}) ->
    io:format("The 'not' expression on line ~p is expected to have the "
	      "following non-boolean type: ~s~n", [P, typelib:pp_type(Ty)]);
handle_type_error({type_error, non_integer_argument_to_bnot, P, Ty}) ->
    io:format("The 'bnot' expression on line ~p has a non-integer argument "
	      " of type ~s~n", [P, typelib:pp_type(Ty)]);
handle_type_error({type_error, bnot_used_with_wrong_type, P, Ty}) ->
    io:format("The 'bnot' expression on line ~p is expected to have the "
	      "following non-integer type: ~s~n", [P, typelib:pp_type(Ty)]);
handle_type_error({type_error, logic_error, LogicOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-boolean argument "
	      "of type ~s~n", [LogicOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, rel_error, LogicOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is used in a context where it is "
	      "required to have type ~s~n", [LogicOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, rel_error, LogicOp, P, Ty1, Ty2}) ->
    io:format("The operator ~p on line ~p is given two arguments with "
	      "non-compatible types:~n~s~n~s~n",
	      [LogicOp, P, typelib:pp_type(Ty1), typelib:pp_type(Ty2)]);
handle_type_error({type_error, list_op_error, ListOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is expected to have "
              "a non-list argument of type ~s~n",
              [ListOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, list_op_error, ListOp, P, Ty, _}) ->
    io:format("The operator ~p on line ~p is given an argument "
              "with a non-list type ~s~n",
              [ListOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, operator_pattern, P, Expr, Ty}) ->
    io:format("The operator pattern ~s on line ~p is expected to have type "
              "~s~n"
             ,[erl_pp:expr(Expr), P, typelib:pp_type(Ty)]);
handle_type_error({type_error, tuple_error, P, Expr, Ty}) ->
    io:format("A tuple {~s} at line ~p didn't match any of the types in the union ~s~n",
              [erl_pp:exprs(Expr), P, typelib:pp_type(Ty)]);
handle_type_error({type_error, pattern, P, Pat, Ty}) ->
    io:format("The pattern ~s on line ~p doesn't have the type ~s~n",
	      [erl_pp:expr(Pat), P, typelib:pp_type(Ty)]);
handle_type_error({type_error, tuple, LINE, Ty}) ->
    io:format("The tuple on line ~p does not have type ~s~n",
	      [LINE, typelib:pp_type(Ty)]);
handle_type_error({unknown_variable, P, Var}) ->
    io:format("Unknown variable ~p on line ~p.~n", [Var, P]);
handle_type_error({type_error, bit_type, Expr, P, Ty1, Ty2}) ->
    io:format("The expression ~s inside the bit expression on line ~p has type ~s "
	      "but the type specifier indicates ~s~n",
	      [erl_pp:expr(Expr), erl_anno:line(P), typelib:pp_type(Ty1), 		       typelib:pp_type(Ty2)]);
handle_type_error({type_error, generator, P, Ty}) ->
    io:format("The generator in a list comprehension on line ~p is expected "
	      "to return a list type, but returns ~s~n",
	      [erl_anno:line(P), typelib:pp_type(Ty)]);
handle_type_error({type_error, check_clauses}) ->
    %%% TODO: Improve quality of type error
    io:format("Type error in clauses");
handle_type_error({type_error, record, P, Record, ResTy}) ->
    io:format("The record #~p on line ~p is expected to have type ~s.~n"
             ,[Record, P, typelib:pp_type(ResTy)]);
handle_type_error({type_error, record_pattern, P, Record, Ty}) ->
    io:format("The record patterns for record #~p on line ~p is expected to have"
              " type ~s.~n"
             ,[Record, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, record_update, P, Record, ResTy}) ->
    io:format("The record update of the record #~p on line ~p is expected to have type:"
              "~n~s~n"
             ,[Record, P, typelib:pp_type(ResTy)]);
handle_type_error({type_error, receive_after, P, TyClauses, TyBlock}) ->
    io:format("The types in the clauses and the after block are incompatible~n"
              "in the receive statement on line ~p.~n"
             "The type of the clauses is : ~s~n"
             "The type of the after block is : ~s~n"
            ,[erl_anno:line(P), typelib:pp_type(TyClauses)
                               , typelib:pp_type(TyBlock)]);
handle_type_error(type_error) ->
    io:format("TYPE ERROR~n").

pp_intersection_type([]) ->
    "";
%% TODO: pp_type seems to have problems printing bounded types.
pp_intersection_type([{type, _, bounded_fun, [Ty, []]} | Tys]) ->
    typelib:pp_type(Ty) ++ pp_intersection_type(Tys);
pp_intersection_type([Ty|Tys]) ->
    typelib:pp_type(Ty) ++ pp_intersection_type(Tys).




line_no(Ty) ->
    element(2,Ty).

-spec gen_partition(integer(), list(tuple()), fun((tuple()) -> {integer(), term()} | false)) ->
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
