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
-record(env, {fenv   = #{}
	     ,venv   = #{}
	     ,tenv   :: #tenv{}
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
-spec normalize(type()) -> type().
normalize(T) -> normalize(T, #tenv{}).

%% TEnv is currently not used. Type definitions are fetched from gradualizer_db.
-spec normalize(type(), TEnv :: #tenv{}) -> type().
normalize({type, _, union, _} = U, TEnv) ->
    Types = flatten_unions([U], TEnv),
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
flatten_unions([{type, _, union, UnionTs} | Ts], TEnv) ->
    UnionTs1 = [normalize(T, TEnv) || T <- UnionTs],
    flatten_unions(UnionTs1 ++ Ts, TEnv);
flatten_unions([T | Ts], TEnv) ->
    [T | flatten_unions(Ts, TEnv)];
flatten_unions([], _TEnv) ->
    [].

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

%% End of subtype help functions

%% Pattern matching on types
%%
%% We sometimes need to pattern match on a type in order to get to
%% its type parameter. One example is the list type, and there are
%% cases where we need to get the type of the elements.

-spec expect_list_type(type()) ->
          {elem_ty,  type()}    %% There is exactly one element type
	| {elem_tys, [type()]}  %% A union can give rise to multiple elem types
	| any                   %% If we don't know the element type
	| {type_error, type()}. %% If the argument is not compatible with lists

expect_list_type({type, _, T, []})
  when T == 'list' orelse T == 'any' orelse
       T == 'nonempty_list' orelse T == 'maybe_improper_list' ->
    any;
expect_list_type({type, _, T, [ElemTy]})
  when T == 'list' orelse T == 'nonempty_list' ->
    {elem_ty, ElemTy};
expect_list_type({type, _, maybe_improper_list, [ElemTy, _]}) ->
    {elem_ty, ElemTy};
expect_list_type({type, _, string, []}) ->
    {elem_ty, {type, erl_anno:new(0), char, []}};
expect_list_type({ann_type, _, [_, Ty]}) ->
    expect_list_type(Ty);
expect_list_type(Union = {type, _, union, UnionTys}) ->
    Tys = lists:flatmap(fun (Ty) ->
				case expect_list_type(Ty) of
				    {type_error, _} ->
					[];
				    any ->
					[{type, erl_anno:new(0), any, []}];
				    {elem_ty, Ty} ->
					[Ty];
				    {elem_tys, Tys} ->
					Tys
				end
			end, UnionTys),
    case Tys of
	[] ->
	    {type_error, Union};
	[Ty] ->
	    {elem_ty, Ty};
	_ ->
	    {elem_tys, Tys}
    end;
expect_list_type({var, _, _Var}) ->
    throw(variable_unimplemented);
expect_list_type(Ty) ->
    {type_error, Ty}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Infer type of expression
%%
%% Type information should always stem from user type annotation (function specs
%% and type annotated record definitions). If an expression does not have a
%% subexpression that has a type inferred from these sources, its inferred type
%% will be `any()'.
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
    {Ty, add_type_pat(Pat, Ty, Env#env.tenv, VarBinds), Cs};
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
        case lists:all(fun({type, _, any, []}) -> true;
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
    % TODO: Should we check the types here?
    case {Ty1, Ty2} of
	{{type, _, any, []}, _} ->
	    {{type, erl_anno:new(0), any, []}, VB2, constraints:empty()};
	{_, {type, _, any, []}} ->
	    {{type, erl_anno:new(0), any, []}, VB2, constraints:empty()};
	{Ty1, TyList = {type, _, list, [Ty]}} ->
	    case subtype(Ty1, Ty, Env#env.tenv) of
		{true, Cs} ->
		    {TyList
		    ,union_var_binds([VB1, VB2])
		    ,constraints:combine([Cs1, Cs2, Cs])};
		false ->
		    throw({type_error, list, 0, Ty1, Ty})
	    end;
	{_, _} ->
	    throw({type_error, list, 0, Ty2})
		% We throw a type error here because Tail is not of type list
		% (nor is it of type any()).
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
    {{type, erl_anno:new(0), any, []},
     union_var_binds(VarBinds),
     constraints:combine(Css)};
type_check_expr(Env, {call, _, Name, Args}) ->
    {FunTy, VarBinds1, Cs1} = type_check_fun(Env, Name, length(Args)),
    {ResTy, VarBinds2, Cs2} = type_check_fun_ty(Env, FunTy, Name, Args),
    {ResTy, union_var_binds([VarBinds1, VarBinds2]), constraints:combine(Cs1, Cs2)};
type_check_expr(Env, {lc, _, Expr, Qualifiers}) ->
    type_check_lc(Env, Expr, Qualifiers);
type_check_expr(Env, {block, _, Block}) ->
    type_check_block(Env, Block);

% Don't return the type of anything other than something
% which ultimately comes from a function type spec.
type_check_expr(_Env, {string, _, _}) ->
    return({type, erl_anno:new(0), any, []});
type_check_expr(_Env, {nil, _}) ->
    return({type, erl_anno:new(0), any, []});
type_check_expr(_Env, {atom, _, _Atom}) ->
    return({type, erl_anno:new(0), any, []});
type_check_expr(_Env, {integer, _, _N}) ->
    return({type, erl_anno:new(0), any, []});
type_check_expr(_Env, {float, _, _F}) ->
    return({type, erl_anno:new(0), any, []});


%% Maps
type_check_expr(Env, {map, _, Assocs}) ->
    type_check_assocs(Env, Assocs);
type_check_expr(Env, {map, _, Expr, Assocs}) ->
    {Ty, VBExpr,   Cs1} = type_check_expr(Env, Expr),
    {Ty, VBAssocs, Cs2} = type_check_assocs(Env, Assocs),
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
    infer_clauses(Env, Clauses);
type_check_expr(Env, {'fun', _, {function, Name, Arity}}) ->
    BoundedFunTypeList = maps:get({Name, Arity}, Env#env.fenv),
    {Ty, Cs} = absform:function_type_list_to_fun_types(BoundedFunTypeList),
    {Ty, #{}, Cs};
type_check_expr(_Env, {'fun', P, {function, {atom, _, M}, {atom, _, F}, {integer, _, A}}}) ->
    case gradualizer_db:get_spec(M, F, A) of
        {ok, BoundedFunTypeList} ->
            {Ty, Cs} = absform:function_type_list_to_fun_types(BoundedFunTypeList),
            {Ty, #{}, Cs};
        not_found ->
            throw({call_undef, P, M, F, A})
    end;

type_check_expr(Env, {'receive', _, Clauses}) ->
    infer_clauses(Env, Clauses);

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
    case subtype({type, P, boolean, []}, Ty, Env#env.tenv) of
	{true, Cs2} ->
	    {{type, erl_anno:new(0), any, []}, VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, non_boolean_argument_to_not, P, Ty})
    end;
type_check_expr(Env, {op, P, BoolOp, Arg1, Arg2}) when
      (BoolOp == 'andalso') or (BoolOp == 'and') or
      (BoolOp == 'orelse')  or (BoolOp == 'or') or (BoolOp == 'xor')  ->
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
      Op == 'bnot' orelse Op == 'div' orelse Op == 'rem' orelse
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
type_check_expr(Env, {'try', _, Block, CaseCs, CatchCs, AfterCs}) ->
    {Ty,  VB,   Cs1}  = type_check_block(Env, Block),
    Env2 = Env#env{ venv = add_var_binds(VB, Env#env.venv) },
    {TyC, _VB2, Cs2} = infer_clauses(Env2, CaseCs),
    {TyS, _VB3, Cs3} = infer_clauses(Env2, CatchCs),
    {TyA, _VB4, Cs4} = infer_clauses(Env2, AfterCs),
    % TODO: Should we check types against each other instead of
    % just merging them?
    % TODO: Check what variable bindings actually should be propagated
    {merge_types([Ty, TyC, TyS, TyA])
    ,VB
    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])}.


type_check_fields(Env, Rec, [{record_field, _, {atom, _, Field}, Expr} | Fields]) ->
    FieldTy = get_rec_field_type(Field, Rec),
    {VB1, Cs1} = type_check_expr_in(Env, FieldTy, Expr),
    {VB2, Cs2} = type_check_fields(Env, Rec, Fields),
    {union_var_binds([VB1, VB2]), constraints:combine(Cs1,Cs2)};
type_check_fields(_Env, _Rec, []) ->
    {#{}, constraints:empty()}.

type_check_logic_op(Env, Op, P, Arg1, Arg2) ->
    % Bindings from the first argument are only passed along for
    % 'andalso' and 'orelse', not 'and' or 'or'.
    UnionVarBindsSecondArg =
	fun (VB1, VB2) ->
		if (Op == 'and') or (Op == 'or') or (Op == 'xor') ->
			VB1;
		   true ->
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
		    {merge_types([Ty1, Ty2])
		    ,union_var_binds([VB1, VB2])
		    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])}
	    end
    end.

type_check_rel_op(Env, Op, P, Arg1, Arg2) ->
    case {type_check_expr(Env, Arg1)
	 ,type_check_expr(Env, Arg2)} of
	{{Ty1, VB1, Cs1}, {Ty2, VB2, Cs2}} ->
	    case {subtype(Ty1, Ty2, Env#env.tenv),
		  subtype(Ty2, Ty1, Env#env.tenv)} of
		{{true, Cs3}, {true, Cs4}} ->
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
		    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])};
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
      {merge_types([Ty1, Ty2])
      ,union_var_binds([VB1, VB2])
      ,constraints:combine([Cs1, Cs2, Cs3, Cs4])
      };
    {false, _} ->
      throw({type_error, list_op_error, Op, P, Ty1, Arg1});
    {_, false} ->
      throw({type_error, list_op_error, Op, P, Ty2, Arg2})
  end.

type_check_fun_ty(Env, {type, _, any, []}, _Name, Args) ->
    { _ArgTys, VarBindsList, Css} =
        lists:unzip3([ type_check_expr(Env, Arg) || Arg <- Args]),
    { {type, erl_anno:new(0), any, []}
    , union_var_binds(VarBindsList)
    , constraints:combine(Css)};
type_check_fun_ty(Env, [{type, _, bounded_fun, [{type, _, 'fun',
				  [{type, _, product, ArgTys}, ResTy]}
				,SCs2]}], _Name, Args) ->
    % TODO: Handle multi-clause function types
    Cs2 = constraints:convert(SCs2),
    {VarBindsList, Css} =
	lists:unzip(
	  lists:zipwith(fun (ArgTy, Arg) ->
			       type_check_expr_in(Env, ArgTy, Arg)
		       end, ArgTys, Args)
	 ),
    { ResTy
    , union_var_binds(VarBindsList)
    , constraints:combine([Cs2 | Css])};
type_check_fun_ty(Env, {type, _, 'fun', [{type, _, 'product', ArgTys}, ResTy]}, _Name, Args) ->
    {VarBindsList, Css} =
	lists:unzip(
	  lists:zipwith(fun (ArgTy, Arg) ->
			       type_check_expr_in(Env, ArgTy, Arg)
		       end, ArgTys, Args)
	 ),
    { ResTy
    , union_var_binds(VarBindsList)
    , constraints:combine(Css)};
type_check_fun_ty(Env, {ann_type, _, [_Var, Ty]}, Name, Arg) ->
    type_check_fun_ty(Env, Ty, Name, Arg).

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
    {_Ty, _VB, Cs} = type_check_expr(Env, Expr),
    % We're returning any() here because we're in a context that doesn't
    % care what type we return. It's different for type_check_lc_in.
    {{type, erl_anno:new(0), any, []}, #{}, Cs};
type_check_lc(Env, Expr, [{generate, P, Pat, Gen} | Quals]) ->
    {Ty,  _,  Cs1} = type_check_expr(Env, Gen),
    case expect_list_type(Ty) of
	{elem_ty, ElemTy} ->
	    {TyL, VB, Cs2} = type_check_lc(Env#env{
					     venv = add_type_pat(Pat
								,ElemTy
								,Env#env.tenv
								,Env#env.venv)
					    }
					  ,Expr, Quals),
	    {TyL, VB, constraints:combine(Cs1,Cs2)};
	any ->
	    {TyL, VB, Cs2} = type_check_lc(Env#env{
					     venv = add_any_types_pat(
						      Pat
						     ,Env#env.venv)
					    }
					  ,Expr, Quals),
	    {TyL, VB, constraints:combine(Cs1,Cs2)};
	{elem_tys, _ElemTys} ->
	    %% TODO: As a hack, we treat a union type as any, just to
	    %% allow the program to type check.
	    {TyL, VB, Cs2} = type_check_lc(Env#env{
					     venv = add_any_types_pat(
						      Pat
						     ,Env#env.venv)
					    }
					  ,Expr, Quals),
	    {TyL, VB, constraints:combine(Cs1,Cs2)};
	{type_error, Ty} ->
	    throw({type_error, generator, P, Ty})
    end.

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
do_type_check_expr_in(Env, Ty, I = {integer, LINE, Int}) ->
    case subtype(I, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, int, Int, LINE, Ty})
    end;
do_type_check_expr_in(Env, Ty, {float, LINE, _Int}) ->
    case subtype(Ty, {type, LINE, float, []}, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, float, LINE, Ty})
    end;
do_type_check_expr_in(Env, Ty, Atom = {atom, LINE, _}) ->
    case subtype(Atom, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, Atom, LINE, Ty})
    end;
do_type_check_expr_in(Env, Ty, Cons = {cons, LINE, H, T}) ->
    case subtype({type, LINE, nonempty_list, [{type, LINE, any, []}]}, Ty, Env#env.tenv) of
	{true, Cs1} ->
	    {VB, Cs2} = type_check_cons_in(Env, Ty, H, T),
	    {VB, constraints:combine(Cs1, Cs2)};
	false ->
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
    case subtype({type, LINE, tuple, lists:duplicate(length(TS), {type, LINE, any, []})}
		,ResTy, Env#env.tenv) of
	false ->
	    throw({type_error, tuple, LINE, ResTy});
	{true, Cs} ->
	    % We re-typecheck the expression for every possible
	    % tuple in a union. That's potentially inefficient.
	    % Maybe we should have a flag to allow for approximation here
	    % in the same way that dialyzer does it.
	    {VBs, Cs2} = type_check_tuple_in(Env, ResTy, TS),
	    {VBs, constraints:combine(Cs, Cs2)}
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
    {Ty, VBAssocs, Cs2} = type_check_assocs(Env, Assocs),
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
do_type_check_expr_in(Env, ResTy, {record, _, Record, Fields}) ->
    case subtype(ResTy, {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]}, Env#env.tenv) of
	{true, Cs} ->
	    Rec = maps:get(Record, Env#env.tenv#tenv.records),
	    {VarBindsList, Css}
		= lists:unzip(
		    lists:map(fun ({record_field, _, {atom, _, Field}, Exp}) ->
				      FieldTy = get_rec_field_type(Field, Rec),
				      type_check_expr_in(Env, FieldTy, Exp)
			      end
			     ,Fields)
		   ),
	    {union_var_binds(VarBindsList), constraints:combine([Cs|Css])};
	false ->
	    %% TODO: Improve quality of error message
	    throw({type_error, record})
    end;
do_type_check_expr_in(Env, ResTy, {record, _, Exp, Record, Fields}) ->
    RecordTy = {type, erl_anno:new(0), record, [{atom, erl_anno:new(0), Record}]},
    case subtype(ResTy, RecordTy, Env#env.tenv) of
	{true, Cs} ->
	    Rec = maps:get(Record, Env#env.tenv#tenv.records),
	    {VarBindsList, Css}
		= lists:unzip(
		    lists:map(fun ({record_field, _, {atom, _, Field}, Expr}) ->
				      FieldTy = get_rec_field_type(Field, Rec),
				      type_check_expr_in(Env, FieldTy, Expr)
			      end
			     ,Fields)
		   ),
	    {VarBinds, Cs} = type_check_expr_in(Env, RecordTy, Exp),
	    {union_var_binds([VarBinds|VarBindsList])
	    ,constraints:combine([Cs|Css])};
	false ->
	    %% TODO: Improve quality of error message
	    throw({type_error, record})
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
    {VB, Cs2} = check_clauses(Env2, ExprTy, ResTy, Clauses),
    {VB, constraints:combine(Cs1,Cs2)};
do_type_check_expr_in(Env, ResTy, {'if', _, Clauses}) ->
    check_clauses(Env, {type, erl_anno:new(0), any, []}, ResTy, Clauses);
do_type_check_expr_in(Env, ResTy, {call, P, Name, Args}) ->
    {FunTy, VarBinds, Cs} = type_check_fun(Env, Name, length(Args)),
    case FunTy of
	{type, _, any, []} ->
	    {_, VarBindsList, Css} =
		lists:unzip3([ type_check_expr(Env, Arg) || Arg <- Args]),
	    { union_var_binds([VarBinds |  VarBindsList])
	    , constraints:combine([Cs|Css]) };
	[{type, _, bounded_fun, [{type, _, 'fun',
				  [{type, _, product, ArgTys}, FunResTy]}
				,SCs2]}] ->
	    % TODO: Handle multi-clause function types
	    Cs2 = constraints:convert(SCs2),
	    {VarBindsList, Css} =
		lists:unzip(
		  lists:zipwith(fun (ArgTy, Arg) ->
				       type_check_expr_in(Env, ArgTy, Arg)
			       end, ArgTys, Args)
		 ),
	    case subtype(FunResTy, ResTy, Env#env.tenv) of
		{true, Cs3} ->
		    { union_var_binds([VarBinds | VarBindsList])
		    , constraints:combine([Cs, Cs2, Cs3 | Css]) };
		false ->
		    throw({type_error, fun_res_type, P, Name, FunResTy, ResTy})
	    end;
	{type, _, 'fun', [{type, _, 'product', ArgTys}, FunResTy]} ->
	    {VarBindsList, Css} =
		lists:unzip(
		  lists:zipwith(fun (ArgTy, Arg) ->
				       type_check_expr_in(Env, ArgTy, Arg)
			       end, ArgTys, Args)
		 ),
	    case subtype(FunResTy, ResTy, Env#env.tenv) of
		{true, Cs3} ->
		    { union_var_binds([VarBinds | VarBindsList])
		    , constraints:combine([Cs, Cs3 | Css]) };
		false ->
		    throw({type_error, fun_res_type, P, Name, FunResTy, ResTy})
	    end
    end;
do_type_check_expr_in(Env, ResTy, {'lc', P, Expr, Qualifiers}) ->
    type_check_lc_in(Env, ResTy, Expr, P, Qualifiers);

%% Functions
do_type_check_expr_in(Env, ResTy, {'fun', _, {clauses, Clauses}}) ->
    check_clauses(Env, ResTy, Clauses);
do_type_check_expr_in(Env, ResTy, {'fun', P, {function, Name, Arity}}) ->
    BoundedFunTypeList = maps:get({Name, Arity}, Env#env.fenv),
    case any_subtype(ResTy, BoundedFunTypeList, Env#env.tenv) of
	{true, Cs} -> {#{}, Cs};
	false -> throw({type_error, fun_res_type, P, {atom, P, Name},
			ResTy, BoundedFunTypeList})
    end;
do_type_check_expr_in(Env, ResTy, {'fun', P, {function, {atom, _, M}, {atom, _, F}, {integer, _, A}}}) ->
    case gradualizer_db:get_spec(M, F, A) of
        {ok, BoundedFunTypeList} ->
	    case any_subtype(ResTy, BoundedFunTypeList, Env#env.tenv) of
		{true, Cs} -> {#{}, Cs};
		false -> throw(fun_error)
	    end;
        not_found ->
            throw({call_undef, P, M, F, A})
    end;

do_type_check_expr_in(Env, ResTy, {'receive', _, Clauses}) ->
    check_clauses(Env, [{type, erl_anno:new(0), any, []}], ResTy, Clauses);
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
	    throw({type_error, not_user_with_wrong_type, P, ResTy})
    end;
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '+' orelse Op == '-' orelse Op == '*' orelse Op == '/' ->
    type_check_arith_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == 'bnot' orelse Op == 'div' orelse Op == 'rem' orelse
      Op == 'band' orelse Op == 'bor' orelse Op == 'bxor' orelse
      Op == 'bsl'  orelse Op == 'bsr' ->
    type_check_int_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == 'and' orelse Op == 'or' orelse Op == 'xor' orelse
      Op == 'andalso' orelse Op == 'orelse' ->
    type_check_logic_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '=:=' orelse Op == '==' orelse
      Op == '>=' orelse Op == '=<' orelse
      Op == '>' orelse Op == '<' ->
    type_check_rel_op_in(Env, ResTy, Op, P, Arg1, Arg2);
do_type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '++' orelse Op == '--' ->
    type_check_list_op_in(Env, ResTy, Op, P, Arg1, Arg2);

do_type_check_expr_in(Env, ResTy, {'catch', _, Arg}) ->
    % TODO: Should we require ResTy to also include the possibility of
    % exceptions? But exceptions can be of any type! That would mean
    % that we require ResTy to be any(), or perhaps also term().
    % But that would make exceptions and types almost incompatible!
    type_check_expr_in(Env, ResTy, Arg);
do_type_check_expr_in(Env, ResTy, {'try', _, Block, CaseCs, CatchCs, AfterCs}) ->
    {VB,   Cs1}  = type_check_block_in(Env, ResTy, Block),
    Env2 = Env#env{ venv = add_var_binds(VB, Env#env.venv) },
    {_VB2, Cs2} = check_clauses(Env2, {type, erl_anno:new(0), any, []}, ResTy, CaseCs),
    {_VB3, Cs3} = check_clauses(Env2, {type, erl_anno:new(0), any, []}, ResTy, CatchCs),
    {_VB4, Cs4} = check_clauses(Env2, {type, erl_anno:new(0), any, []}, ResTy, AfterCs),
    % TODO: Check what variable bindings actually should be propagated
    {VB
    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])}.


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
	  {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
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
    case expect_list_type(ResTy) of
	any ->
	    {_Ty, _VB, Cs} = type_check_expr(Env, Expr),
	    {#{}, Cs};
	{elem_ty, ElemTy} ->
	    {_VB, Cs} = type_check_expr_in(Env, ElemTy, Expr),
	    {#{}, Cs};
	{elem_tys, _ElemTys} ->
	    %% TODO: As a hack, we treat a union type as any, just to
	    %% allow the program to type check.
	    {_Ty, _VB, Cs} = type_check_expr(Env, Expr),
	    {#{}, Cs};
	{type_error, Ty} ->
	    throw({type_error, lc, P, Ty})
    end;
type_check_lc_in(Env, ResTy, Expr, P, [{generate, P_Gen, Pat, Gen} | Quals]) ->
    {Ty, _VB1, Cs1} = type_check_expr(Env, Gen),
    case expect_list_type(Ty) of
	any ->
	    {_VB2, Cs2} = type_check_lc_in(Env#env{
					     venv =
						 add_any_types_pat(Pat
								  ,Env#env.venv)
					    }
					  ,ResTy, Expr, P, Quals),
	    {#{}, constraints:combine(Cs1, Cs2)};
	{elem_ty, ElemTy} ->
	    {_VB2, Cs2} = type_check_lc_in(Env#env{
					     venv =
						 add_type_pat(Pat
							     ,ElemTy
							     ,Env#env.tenv
							     ,Env#env.venv)
					    }
					  ,ResTy, Expr, P, Quals),
	    {#{}, constraints:combine(Cs1, Cs2)};
	{elem_tys, _ElemTys} ->
	    %% TODO: As a hack, we treat a union type as any, just to
	    %% allow the program to type check.
	    {_VB2, Cs2} = type_check_lc_in(Env#env{
					     venv =
						 add_any_types_pat(Pat
								  ,Env#env.venv)
					    }
					  ,ResTy, Expr, P, Quals),
	    {#{}, constraints:combine(Cs1, Cs2)};
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


% TODO: Collect constraints
type_check_fun(Env, {atom, P, Name}, Arity) ->
    % Local function call
    case maps:find({Name, Arity}, Env#env.fenv) of
	{ok, Types} ->
	    {Types, #{}, constraints:empty()};
	error ->
	    case erl_internal:bif(Name, Arity) of
		true ->
		    {ok, Types} = gradualizer_db:get_spec(erlang, Name, Arity),
		    {Types, #{}, constraints:empty()};
		false ->
		    throw({call_undef, P, Name, Arity})
	    end
    end;
type_check_fun(_Env, {remote, P, {atom,_,Module}, {atom,_,Fun}}, Arity) ->
    % Module:function call
    case gradualizer_db:get_spec(Module, Fun, Arity) of
	{ok, Types} -> {Types, #{}, constraints:empty()};
	not_found   -> throw({call_undef, P, Module, Fun, Arity})
    end;
type_check_fun(Env, Expr, _Arity) ->
    type_check_expr(Env, Expr).

type_check_block(Env, [Expr]) ->
    type_check_expr(Env, Expr);
type_check_block(Env, [Expr | Exprs]) ->
    {_, VarBinds, Cs1} = type_check_expr(Env, Expr),
    {Ty, VB, Cs2} = type_check_block(Env#env{ venv = add_var_binds(Env#env.venv, VarBinds) }, Exprs),
    {Ty, VB, constraints:combine(Cs1, Cs2)}.

type_check_block_in(Env, ResTy, [Expr]) ->
    type_check_expr_in(Env, ResTy, Expr);
type_check_block_in(Env, ResTy, [Expr | Exprs]) ->
    {_, VarBinds, Cs1} = type_check_expr(Env, Expr),
    {VB, Cs2} = type_check_block_in(Env#env{ venv = add_var_binds(Env#env.venv, VarBinds) }, ResTy, Exprs),
    {VB, constraints:combine(Cs1, Cs2)}.


type_check_tuple_in(Env, {type, _, tuple, any}, TS) ->
    {VBs, Css} = lists:unzip(
      lists:map(fun (Expr) ->
			type_check_expr_in(Env, {type, erl_anno:new(0), any, []}, Expr)
		end, TS)),
    {union_var_binds(VBs), constraints:combine(Css)};
type_check_tuple_in(Env, {type, _, tuple, Tys}, TS) ->
    {VBs, Css} = lists:unzip(
      lists:zipwith(fun (Ty, Expr) ->
			type_check_expr_in(Env, Ty, Expr)
		end, Tys, TS)),
    {union_var_binds(VBs), constraints:combine(Css)};
type_check_tuple_in(Env, {type, _, union, Tys} = UTy, TS) ->
    try type_check_tuple_union(Env, Tys, TS)
    catch {type_error, tuple_error} ->
            P = element(2, hd(TS)),
            throw({type_error, tuple_error, P, TS, UTy})
    end;
type_check_tuple_in(Env, {ann_type, _, [_, Ty]}, TS) ->
    type_check_tuple_in(Env, Ty, TS);
type_check_tuple_in(Env, {var, _, Name}, TS) ->
    {Tys, VarBindsList, Css} =
	lists:unzip3(lists:map(fun (Expr) ->
				   type_check_expr(Env, Expr)
			       end, TS)),
    {union_var_binds(VarBindsList)
    ,constraints:combine([constraints:upper(Name, {type, erl_anno:new(0), tuple, Tys})| Css])
    };
type_check_tuple_in(Env, Ty, TS) when element(1, Ty) == user_type;
                                      element(1, Ty) == remote_type ->
    NormTy = normalize(Ty, Env#env.tenv),
    type_check_tuple_in(Env, NormTy, TS).


type_check_tuple_union(Env, [Tuple = {type, _, tuple, _}|Union], TS) ->
    try type_check_tuple_in(Env, Tuple, TS)
    catch
	_ ->
	    type_check_tuple_union(Env, Union, TS)
    end;
type_check_tuple_union(Env, [_|Union], TS) ->
    type_check_tuple_union(Env, Union, TS);
type_check_tuple_union(_Env, [], _TS) ->
    throw({type_error, tuple_error}).

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



infer_clauses(Env, Clauses) ->
    {Tys, VarBindsList, Css} =
	lists:unzip3(lists:map(fun (Clause) ->
				       infer_clause(Env, Clause)
			       end, Clauses)),
    {merge_types(Tys), union_var_binds(VarBindsList), constraints:combine(Css)}.

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

check_clauses(_Env, [], _Clauses) ->
    %% TODO: Improve quality of type error
    throw({typer_error, check_clauses});
check_clauses(Env, [{type, _, 'fun', [{type, _, product, ArgsTy},ResTy]}|Tys], Clauses) ->
    try
	check_clauses(Env, ArgsTy, ResTy, Clauses)
    catch
	_ ->
	    check_clauses(Env, Tys, Clauses)
    end;
check_clauses(Env, [{var, _, TyVar}|_Tys], Clauses) ->
    %%% We don't backtrack here in case of a type error, because the
    %%% type error is not due to us pushing in a type. Hence, intersection
    %%% types with type variables are rather weak in this case.
    {Ty, VarBinds, Cs} = infer_clauses(Env, Clauses),
    {VarBinds, constraints:combine(constraints:upper(TyVar, Ty), Cs)};
check_clauses(Env, {type, _, 'fun', [{type, _, product, ArgsTy}, ResTy]}, Clauses) ->
    check_clauses(Env, ArgsTy, ResTy, Clauses);
check_clauses(Env, {var, _, TyVar}, Clauses) ->
    {Ty, VarBinds, Cs} = infer_clauses(Env, Clauses),
    {VarBinds, constraints:combine(constraints:upper(TyVar, Ty), Cs)}.



check_clauses(Env, ArgsTy, ResTy, Clauses) when
      not is_list(ArgsTy) ->
    check_clauses(Env, [ArgsTy], ResTy, Clauses);
check_clauses(Env, ArgsTy, ResTy, Clauses) ->
    {VarBindsList, Css} =
	lists:unzip(lists:map(fun (Clause) ->
				  check_clause(Env, ArgsTy, ResTy, Clause)
			  end, Clauses)),
    {union_var_binds(VarBindsList), constraints:combine(Css)}.

check_clause(Env, ArgsTy, ResTy, {clause, _, Args, Guards, Block}) ->
    case length(ArgsTy) =:= length(Args) of
	false ->
	    throw(argument_length_mismatch);
	true ->
	    EnvNew    = Env#env{ venv = add_types_pats(Args,
	                                               ArgsTy,
	                                               Env#env.tenv,
	                                               Env#env.venv) },
	    VarBinds  = check_guards(EnvNew, Guards),
	    EnvNewest = EnvNew#env{ venv = add_var_binds(EnvNew#env.venv, VarBinds) },
	    type_check_block_in(EnvNewest, ResTy, Block)
    end;
%% DEBUG
check_clause(_Env, _ArgsTy, _ResTy, Term) ->
    io:format("DEBUG: check_clause term: ~p~n", [Term]),
    throw(check_clause).


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
    {ok, [{type, _, bounded_fun, [{type, _, 'fun',
                                   [{type, _, product, ArgsTy}, ResTy]},
                                  SCs2]}]} ->
	    % TODO: Handle multi-clause function types
	    Cs2 = constraints:convert(SCs2),
	    NormResTy  = normalize(ResTy, Env#env.tenv),
	    NormArgsTy = lists:map(fun (ArgTy) ->
					   normalize(ArgTy, Env#env.tenv)
				   end
				  ,ArgsTy),
	    {VarBinds, Cs} = check_clauses(Env,
					   NormArgsTy, NormResTy, Clauses),
	    {ResTy, VarBinds, constraints:combine(Cs,Cs2)};
	{ok, {type, _, any, []}} ->
	    infer_clauses(Env, Clauses);
	error ->
	    throw({internal_error, missing_type_spec, Name, NArgs})
    end.

merge_types([]) ->
    {type, erl_anno:new(0), any, []};
merge_types([Ty]) ->
    Ty;
merge_types(Tys) ->
    %%% TODO: We shouldn't be so eager to convert types to any().
    %%% If we find any() in the list, it should simply vanish, if favour of
    %%% the other types present in the list.
    case lists:keyfind(any, 3, Tys) of
	Any = {type, _, any, []} ->
	    Any;
	_ ->
	    case Tys of
		[Ty={atom, _, A}, {atom, _, A} | Rest] ->
		    merge_types([Ty | Rest]);
		[{atom, _, false}, {atom, _, true} | Rest] ->
		    merge_types([{type, erl_anno:new(0), boolean, []} | Rest]);
		[{atom, _, true}, {atom, _, false} | Rest] ->
		    merge_types([{type, erl_anno:new(0), boolean, []} | Rest]);
		[{atom, _, _}, {type, _, _, _} | _] ->
		    {type, erl_anno:new(0), any, []};
		[{type, P, Ty, Args1}, {type, _, Ty, Args2}]
		  when length(Args1) == length(Args2) ->
		    {type, P, Ty, lists:zipwith(fun (A,B) -> merge_types([A,B])
						end, Args1, Args2)};
		[{type, P, tuple, Args1}, {type, _, tuple, Args2} | Rest] ->
		    case length(Args1) == length(Args2) of
			false ->
			    {type, erl_anno:new(0), any, []};
			true  ->
			    merge_types([{type, P, tuple,
					  lists:zipwith(fun (A1, A2) ->
								merge_types([A1,A2]) end,
							Args1, Args2)}
					 | Rest])
		    end;
		[{type, _, map, Assocs}, {type, _, map, Assocs}] ->
		    % TODO: Figure out how to merge field assocs properly
		    [{type, erl_anno:new(0), map, []}]
	    end
    end.

add_types_pats([], [], _TEnv, VEnv) ->
    VEnv;
add_types_pats([Pat | Pats], [Ty | Tys], TEnv, VEnv) ->
    NormTy = normalize(Ty, TEnv),
    ?throw_orig_type(add_types_pats(Pats, Tys, TEnv, add_type_pat(Pat, NormTy, TEnv, VEnv)),
                     Ty, NormTy).

add_type_pat({var, _, '_'}, _Ty, _TEnv, VEnv) ->
    VEnv;
add_type_pat({var, _, A}, Ty, _TEnv, VEnv) ->
    VEnv#{ A => Ty };
add_type_pat(Expr, {type, _, any, []}, _TEnv, VEnv) ->
    add_any_types_pat(Expr, VEnv);
add_type_pat({integer, _, _}, _Ty, _TEnv, VEnv) ->
    VEnv;
add_type_pat(Tuple = {tuple, P, Pats}, Ty, TEnv, VEnv) ->
    case subtype({type,P,tuple,lists:duplicate(length(Pats),{type,0,any,[]})},
		 Ty, TEnv) of
	{true, _Cs} ->
	    add_type_pat_tuple(Pats, Ty, TEnv, VEnv);
	false ->
	    throw({type_error, pattern, P, Tuple, Ty})
    end;
add_type_pat(Atom = {atom, P, _}, Ty, TEnv, VEnv) ->
    case subtype(Atom, Ty, TEnv) of
	% There cannot be any constraints generated in this case
	{true, _Cs} ->
	    VEnv;
	false ->
	    throw({type_error, pattern, P, Atom, Ty})
    end;
add_type_pat(Nil = {nil, P}, Ty, TEnv, VEnv) ->
    case subtype({type, P, nil, []}, Ty, TEnv) of
	% There cannot be any constraints generated in this case
	{true, _Cs} ->
	    VEnv;
	false ->
	    throw({type_error, pattern, P, Nil, Ty})
    end;
add_type_pat(CONS = {cons, P, PH, PT}, ListTy, TEnv, VEnv) ->
    case expect_list_type(ListTy) of
	any ->
            VEnv2 = add_any_types_pat(PH, VEnv),
            add_type_pat(PT, ListTy, TEnv, VEnv2);
	{elem_ty, ElemTy} ->
	    VEnv2 = add_type_pat(PH, ElemTy, TEnv, VEnv),
            add_type_pat(PT, ListTy, TEnv, VEnv2);
	{elem_tys, _ElemTys} ->
	    %% TODO: As a hack, we treat a union type as any, just to
	    %% allow the program to type check.
            VEnv2 = add_any_types_pat(PH, VEnv),
            add_type_pat(PT, ListTy, TEnv, VEnv2);
	{type_error, _Ty} ->
	    throw({type_error, P, CONS, ListTy})
    end;
add_type_pat(String = {string, P, _}, Ty, _TEnv, VEnv) ->
   case subtype({type, P, string, []}, Ty, VEnv) of
     %% TODO: We should propagate the constraints here
     {true, _Cs} ->
       VEnv;
     false ->
       throw({type_error, pattern, P, String, Ty})
   end;
add_type_pat({bin, _, BinElements}, {type, _, binary, [_,_]}, TEnv, VEnv) ->
    %% TODO: Consider the bit size parameters
    lists:foldl(fun ({bin_element, _, Pat, _Size, Specifiers}, VEnv1) ->
			%% Check Pat against the bit syntax type specifiers
			SpecTy = bit_specifier_list_to_type(Specifiers),
			add_type_pat(Pat, SpecTy, TEnv, VEnv1)
		end,
		VEnv,
		BinElements);
add_type_pat({record, _, _Record, Fields}, {type, _, record, [{atom, _, _RecordName}]}, TEnv, VEnv) ->
    % TODO: We need the definitions of records here, to be able to add the
    % types of the matches in the record.
    add_type_pat_fields(Fields, {type, erl_anno:new(0), any, []}, TEnv, VEnv);
add_type_pat({match, _, Pat1, Pat2}, Ty, TEnv, VEnv) ->
    add_type_pat(Pat1, Ty, TEnv, add_type_pat(Pat2, Ty, TEnv, VEnv));

add_type_pat(Pat, {ann_type, _, [_, Ty]}, TEnv, VEnv) ->
    add_type_pat(Pat, Ty, TEnv, VEnv);

add_type_pat(Pat, Ty, _TEnv, _VEnv) ->
    throw({type_error, pattern, element(2, Pat), Pat, Ty}).

add_type_pat_fields([], _, _TEnv, VEnv) ->
    VEnv;
add_type_pat_fields([{record_field, _, _Field, Pat}|Fields], Ty, TEnv, VEnv) ->
    add_type_pat_fields(Fields, Ty, TEnv, add_type_pat(Pat, Ty, TEnv, VEnv)).



add_type_pat_list([Pat|Pats], [Ty|Tys], TEnv, VEnv) ->
    VEnv2 = add_type_pat(Pat, Ty, TEnv, VEnv),
    add_type_pat_list(Pats, Tys, TEnv, VEnv2);
add_type_pat_list([], [], _TEnv, VEnv) ->
    VEnv.

add_type_pat_tuple(Pats, {type, _, any, []}, _TEnv, VEnv) ->
    add_any_types_pats(Pats, VEnv);
add_type_pat_tuple(Pats, {type, _, tuple, any}, _TEnv, VEnv) ->
    add_any_types_pats(Pats, VEnv);
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
    lists:foldl(fun ({Pat, Union}, Env) ->
			add_type_pat(Pat, Union, TEnv, Env)
		end, VEnv, lists:zip(Pats, Unions));
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
add_any_types_pat({var, _,'_'}, VEnv) ->
    VEnv;
add_any_types_pat({var, _,A}, VEnv) ->
    VEnv#{ A => {type, erl_anno:new(0), any, []} }.

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

union_var_binds([]) ->
    #{};
union_var_binds([ VarBinds | VarBindsList ]) ->
    gradualizer_lib:merge_with(fun glb_types/3, VarBinds, union_var_binds(VarBindsList)).

add_var_binds(VEnv, VarBinds) ->
    gradualizer_lib:merge_with(fun glb_types/3, VEnv, VarBinds).

% TODO: improve
% Is this the right function to use or should I always just return any()?
glb_types(K, {type, _, N, Args1}, {type, _, N, Args2}) ->
    Args = [ glb_types(K, Arg1, Arg2) || {Arg1, Arg2} <- lists:zip(Args1, Args2) ],
    {type, erl_anno:new(0), N, Args};
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
    Env = create_env(ParseData),
    lists:foldr(fun (Function, Res) when Res =:= ok;
                                         not StopOnFirstError ->
			try type_check_function(Env, Function) of
			    {_Ty, _VarBinds, _Cs} ->
				Res
			catch
			    Throw ->
				% Useful for debugging
				% io:format("~p~n", [erlang:get_stacktrace()]),
				File =/= undefined andalso io:format("~s: ", [File]),
				handle_type_error(Throw),
				nok;
			    error:Error ->
				%% A hack to hide the (very large) #env{} in
				%% error stacktraces. TODO: Add an opt for this.
				Trace = case erlang:get_stacktrace() of
				    [{M, F, [#env{}|Args], Pos} | RestTrace] ->
					[{M, F, ['*environment excluded*', Args], Pos} | RestTrace];
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
                     }) ->
    FEnv = create_fenv(Specs, Funs),
    TEnv = create_tenv(Types ++ Opaques, Records),
    #env{ fenv = FEnv, tenv = TEnv }.

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
    maps:from_list([ {{Name, NArgs}, {type, erl_anno:new(0), any, []}}
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
handle_type_error({type_error, tyVar, LINE, Var, VarTy, Ty}) ->
    io:format("The variable ~p on line ~p has type ~s "
	      "but is expected to have type ~s~n",
	      [Var, LINE, typelib:pp_type(VarTy), typelib:pp_type(Ty)]);
handle_type_error({type_error, {atom, _, A}, LINE, Ty}) ->
    io:format("The atom ~p on line ~p does not have type ~s~n",
	      [A, LINE, typelib:pp_type(Ty)]);
handle_type_error({type_error, string, LINE, String, Ty}) ->
    io:format("The string ~p on line ~p does not have type ~s~n",
              [String, LINE, typelib:pp_type(Ty)]);
handle_type_error({type_error, int, I, LINE, Ty}) ->
    io:format("The integer ~p on line ~p does not have type ~s~n",
	      [I, LINE, typelib:pp_type(Ty)]);
handle_type_error({type_error, compat, _LINE, Ty1, Ty2}) ->
    io:format("The type ~s is not compatible with type ~s~n"
	     ,[typelib:pp_type(Ty1), typelib:pp_type(Ty2)]);
handle_type_error({type_error, list, _, Ty1, Ty}) ->
    io:format("The type ~s cannot be an element of a list of type ~s~n",
	      [typelib:pp_type(Ty1), typelib:pp_type(Ty)]);
handle_type_error({type_error, list, _, Ty}) ->
    io:format("The type ~s on line ~p is not a list type~n",
	      [typelib:pp_type(Ty), line_no(Ty)]);
handle_type_error({type_error, nil, LINE, Ty}) ->
    io:format("The empty list on line ~p does not have type ~s~n",
	      [LINE, typelib:pp_type(Ty)]);
handle_type_error({type_error, call, _P, Name, TyArgs, ArgTys}) ->
    io:format("The function ~p expects arguments of type~n~p~n but is given "
	      "arguments of type~n~p~n",
	      [Name, TyArgs, ArgTys]);
handle_type_error({type_error, fun_res_type, P, Func, FunResTy, ResTy}) ->
    Name = erl_pp:expr(Func), %% {atom, _, Name} or {remote, Mod, Name}
    io:format("The function ~s in line ~p is expected to return ~s but it returns ~s~n",
              [Name, P, typelib:pp_type(ResTy), typelib:pp_type(FunResTy)]);
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
handle_type_error(type_error) ->
    io:format("TYPE ERROR~n").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec store_interface_file(map(), #parsedata{}) -> ok | {error, term()}.
store_interface_file(FEnv, ParseData) ->
    Filename = io_lib:format("~p.Gr", [ParseData#parsedata.module]),
    FEnv1 = remove_unexported(FEnv, ParseData),
    FileContents = io_lib:format("~p.~n", [FEnv1]),
    file:write_file(Filename, FileContents).

%% Remove unexported function from an FEnv
-spec remove_unexported(map(), #parsedata{}) -> map().
remove_unexported(FEnv, #parsedata{export_all = true}) ->
    FEnv;
remove_unexported(FEnv, #parsedata{exports = Exports}) ->
    maps:filter(fun (Key, _Value) ->
			lists:member(Key, Exports)
		end,
		FEnv).

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
