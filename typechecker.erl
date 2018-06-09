-module(typechecker).

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
                               {record_field, erl_anno:anno(), Name :: atom(),
                                DefaultValue :: erl_parse:abstract_expr()},
     			        Type :: erl_parse:abstract_type()}.

%% Type environment, passed around while comparing compatible subtypes
-record(tenv, {types   :: #{{Name :: atom(), arity()} => {Params :: [atom()],
							  Body :: type()}},
	       records :: #{Name :: atom()            => [typed_record_field()]}
	      }).

%%% The environment passed around during typechecking.
-record(env, {fenv   = #{}
	     ,venv   = #{}
	     ,tenv   :: #tenv{}
	     %,tyvenv = #{}
	     }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subtyping compatibility
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The first argument is a "compatible subtype" of the second.

-spec subtype(type(), type(), #tenv{}) -> {true, any()} | false.
subtype(Ty1, Ty2, TEnv) ->
    try compat(typelib:remove_pos(Ty1), typelib:remove_pos(Ty2),
               sets:new(), TEnv) of
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


% This function throws an exception in case of a type error

%% The functions compat and compat_ty are mutually recursive.
%% The main entry point is compat and all recursive calls should go via compat.
%% The function compat_ty is just a convenience function to be able to
%% pattern match on types in a nice way.
compat(T1, T2, A, TEnv) ->
    Ty1 = normalize(T1, TEnv),
    Ty2 = normalize(T2, TEnv),
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

% Integer types
compat_ty({type, _, range, _}, {type, _, integer, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, range, [{integer, _, I11}, {integer, _, I12}]},
	  {type, _, range, [{integer, _, I21}, {integer, _, I22}]},
	  A, _TEnv) when
      I11 >= I21 andalso I12 =< I22 ->
    ret(A);
compat_ty({integer, _, I}, {integer, _, I}, A, _TEnv) ->
    ret(A);
compat_ty({integer, _, _I}, {type, _, integer, []}, A, _TEnv) ->
    ret(A);
compat_ty({integer,_,  I}, {type, _, range, [{integer, _, I1}, {integer, _, I2}]}, A, _TEnv)
  when I >= I1 andalso I =< I2 ->
    ret(A);
compat_ty({type, _, pos_integer, []}, {type, _, integer, []}, A, _TEnv) ->
    ret(A);
compat_ty({integer, _, I}, {type, _, pos_integer, []}, A, _TEnv)
  when I > 0 ->
    ret(A);
compat_ty({type, _, range, [{integer, _, I1}, {integer, _, I2}]},
	  {type, _, pos_integer, []}, A, _TEnv)
  when I1 > 0 andalso I2 > 0 ->
    ret(A);
compat_ty({type, _, neg_integer, []}, {type, _, integer, []}, A, _TEnv) ->
    ret(A);
compat_ty({integer, _, I}, {type, _, neg_integer, []}, A, _TEnv)
  when I < 0 ->
    ret(A);
compat_ty({type, _, range, [{integer, _, I1}, {integer, _, I2}]},
	  {type, _, neg_integer, []}, A, _TEnv)
  when I1 < 0 andalso I2 < 0 ->
    ret(A);
compat_ty({type, _, non_neg_integer, []}, {type, _, integer, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, _, pos_integer, []}, {type, _, non_neg_integer, []}, A, _TEnv) ->
    ret(A);
compat_ty({integer, _, I}, {type, _, non_neg_integer, []}, A, _TEnv)
  when I >= 0 ->
    ret(A);
compat_ty({type, _, range, [{integer, _, I1}, {integer, _, I2}]},
	  {type, _, non_neg_integer, []}, A, _TEnv)
  when I1 >= 0 andalso I2 >= 0 ->
    ret(A);

%% Atoms
compat_ty({atom, _, _Atom}, {type, _, atom, []}, A, _TEnv) ->
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
compat_ty({nil, _}, {type, _, list, _Any}, A, _TEnv) ->
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
compat_ty({nil, _}, {type, _, maybe_improper_list, [_Ty2]}, A, _TEnv) ->
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
normalize(T) -> normalize(T, #{}).

%% TEnv is currently not used. Type definitions are fetched from gradualizer_db.
-spec normalize(type(), TEnv :: map()) -> type().
normalize({type, _, union, _} = U, TEnv) ->
    Types = flatten_unions([U], TEnv),
    case merge_union_types(Types, TEnv) of
        []  -> {type, 0, none, []};
        [T] -> T;
        Ts  -> {type, 0, union, Ts}
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
                    typelib:substitute_type_vars(Type0, VarMap);
                _NotFound ->
                    throw({undef, user_type, {Name, length(Args)}})
            end
    end;
normalize({remote_type, _P, Module, Name, Args} = RemoteType, TEnv) ->
    case gradualizer_db:get_exported_type(Module, Name, Args) of
        {ok, T} ->
            normalize(T, TEnv);
        opaque ->
            RemoteType;
        not_exported ->
            throw({not_exported, remote_type, {Module, Name, length(Args)}});
        not_found ->
            throw({undef, remote_type, {Module, Name, length(Args)}})
    end;
normalize({op, _, '-', {integer, Ann, I}}, _TEnv) ->
    {integer, Ann, -I};
normalize({op, _, '+', {integer, Ann, I}}, _TEnv) ->
    {integer, Ann, I};
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
    {type, Ann, maybe_improper_list, [{union, Union}]};
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
-spec flatten_unions([type()], map()) -> [type()].
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
            [{type, 0, term, []}];
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
    [{type, 0, integer, []}];
int_range_to_types({neg_inf, -1}) ->
    [{type, 0, neg_integer, []}];
int_range_to_types({neg_inf, 0}) ->
    [{type, 0, neg_integer, []}, {integer, 0, 0}];
int_range_to_types({neg_inf, I}) when I > 0 ->
    [{type, 0, neg_integer, []},
     {type, 0, range, [{integer, 0, 0}, {integer, 0, I}]}];
int_range_to_types({I, pos_inf}) when I < -1 ->
    [{type, 0, range, [{integer, 0, I}, {integer, 0, -1}]},
     {type, 0, non_neg_integer, []}];
int_range_to_types({-1, pos_inf}) ->
    [{integer, 0, -1}, {type, 0, non_neg_integer, []}];
int_range_to_types({0, pos_inf}) ->
    [{type, 0, non_neg_integer, []}];
int_range_to_types({1, pos_inf}) ->
    [{type, 0, pos_integer, []}];
int_range_to_types({I, I}) ->
    [{integer, 0, I}];
int_range_to_types({I, J}) when I < J ->
    [{type, 0, range, [{integer, 0, I}, {integer, 0, J}]}].

%% End of subtype help functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Arguments: An environment for functions, an environment for variables
%% and the expression to type check.
%% Returns the type of the expression and a collection of variables bound in
%% the expression together with their type.
%-spec type_check_expr(#env, any()) -> { any(), #{ any() => any()} }.
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
type_check_expr(_Env, {integer, _, _N}) ->
    return({type, 0, any, []});
type_check_expr(Env, {tuple, _, TS}) ->
    { Tys, VarBinds, Css} = lists:unzip3([ type_check_expr(Env, Expr)
				        || Expr <- TS ]),
    { {type, 0, tuple, Tys}, union_var_binds(VarBinds), constraints:combine(Css) };
type_check_expr(Env, {cons, _, Head, Tail}) ->
    {Ty1, VB1, Cs1} = type_check_expr(Env, Head),
    {Ty2, VB2, Cs2} = type_check_expr(Env, Tail),
    % TODO: Should we check the types here?
    case {Ty1, Ty2} of
	{{type, _, any, []}, _} ->
	    {{type, 0, any, []}, VB2, constraints:empty()};
	{_, {type, _, any, []}} ->
	    {{type, 0, any, []}, VB2, constraints:empty()};
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
type_check_expr(Env, {call, _, Name, Args}) ->
    {FunTy, VarBind, Cs} = type_check_fun(Env, Name, length(Args)),
    case FunTy of
	{type, _, any, []} ->
	    { _ArgTys, VarBinds, Css} =
		lists:unzip3([ type_check_expr(Env, Arg) || Arg <- Args]),
	    { {type, 0, any, []}
	    , union_var_binds([VarBind | VarBinds])
	    , constraints:combine([Cs | Css])};
	[{type, _, 'fun', [{type, _, product, ArgTys}, ResTy]}] ->
	    % TODO: Handle multi-clause function types
	    {VarBinds, Css} =
		lists:unzip(
		  lists:zipwith(fun (ArgTy, Arg) ->
				       type_check_expr_in(Env, ArgTy, Arg)
			       end, ArgTys, Args)
		 ),
	    { ResTy
	    , union_var_binds([VarBind | VarBinds])
	    , constraints:combine([Cs | Css])};
	[{type, _, bounded_fun, [{type, _, 'fun',
				  [{type, _, product, ArgTys}, ResTy]}
				,SCs2]}] ->
	    Cs2 = constraints:convert(SCs2),
	    {VarBinds, Css} =
		lists:unzip(
		  lists:zipwith(fun (ArgTy, Arg) ->
				       type_check_expr_in(Env, ArgTy, Arg)
			       end, ArgTys, Args)
		 ),
	    { ResTy
	    , union_var_binds([VarBind | VarBinds])
	    , constraints:combine([Cs, Cs2 | Css])}
    end;
type_check_expr(Env, {lc, _, Expr, Qualifiers}) ->
    type_check_lc(Env, Expr, Qualifiers);
type_check_expr(Env, {block, _, Block}) ->
    type_check_block(Env, Block);

% Don't return the type of anything other than something
% which ultimately comes from a function type spec.
type_check_expr(_Env, {string, _, _}) ->
    return({type, 0, any, []});
type_check_expr(_Env, {nil, _}) ->
    return({type, 0, any, []});
type_check_expr(_Env, {atom, _, _Atom}) ->
    return({type, 0, any, []});

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
    {VB, Cs} = type_check_expr_in(Env, {type, 0, record, [{atom, 0, Record}]}, Expr),
    Rec = maps:get(Record, Env#env.tenv#tenv.records),
    Ty  = maps:get(Field, Rec),
    {Ty, VB, Cs};
type_check_expr(Env, {record, _, Expr, Record, Fields}) ->
    RecTy = {type, 0, record, [{atom, 0, Record}]},
    {VB1, Cs1} = type_check_expr_in(Env, RecTy, Expr),
    Rec = maps:get(Record, Env#env.tenv#tenv.records),
    {VB2, Cs2} = type_check_fields(Env, Rec, Fields),
    {RecTy, union_var_binds([VB1, VB2]), constraints:combine(Cs1, Cs2)};
type_check_expr(Env, {record, _, Record, Fields}) ->
    RecTy    = {type, 0, record, [{atom, 0, Record}]},
    Rec      = maps:get(Record, Env#env.tenv#tenv.records),
    {VB, Cs} = type_check_fields(Env, Rec, Fields),
    {RecTy, VB, Cs};

%% Functions
type_check_expr(Env, {'fun', _, {clauses, Clauses}}) ->
    infer_clauses(Env, Clauses);
type_check_expr(Env, {'fun', _, {function, Name, Arity}}) ->
    return(maps:get({Name, Arity}, Env#env.fenv));

type_check_expr(Env, {'receive', _, Clauses}) ->
    infer_clauses(Env, Clauses);

%% Operators
type_check_expr(Env, {op, _, '!', Proc, Val}) ->
    % Message passing is always untyped, which is why we discard the types
    {_, VB1, Cs1} = type_check_expr(Env, Proc),
    {_, VB2, Cs2} = type_check_expr(Env, Val),
    {{type, 0, any, []}
    ,union_var_binds([VB1, VB2])
    ,constraints:combine(Cs1, Cs2)};
type_check_expr(Env, {op, P, 'not', Arg}) ->
    {Ty, VB, Cs1} = type_check_expr(Env, Arg),
    case subtype({type, 0, boolean, []}, Ty, Env#env.tenv) of
	{true, Cs2} ->
	    {{type, 0, any, []}, VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, non_boolean_argument_to_not, P, Ty})
    end;
type_check_expr(Env, {op, P, BoolOp, Arg1, Arg2}) when
      (BoolOp == 'andalso') or (BoolOp == 'and') or
      (BoolOp == 'orelse')  or (BoolOp == 'or') ->
    % Bindings from the first argument are only passed along for
    % 'andalso' and 'orelse', not 'and' or 'or'.
    UnionVarBindsSecondArg =
	fun (VB1, VB2) ->
		if (BoolOp == 'and') or (BoolOp == 'or') ->
			VB1;
		   true ->
			union_var_binds([VB1, VB2])
		end
	end,
    case type_check_expr(Env, Arg1) of
	{Ty1, VB1, Cs1} ->
	    case subtype(Ty1, {type, 0, bool, []}, Env#env.tenv) of
		false ->
		    throw({type_error, boolop, BoolOp, P, Ty1});
		{true, Cs2} ->
		    case type_check_expr(Env#env{ venv = UnionVarBindsSecondArg(Env#env.venv,VB1 )}, Arg2) of
			{Ty2, VB2, Cs3} ->
			    case subtype(Ty2, {type, 0, bool, []}, Env#env.tenv) of
				false ->
				    throw({type_error, boolop, BoolOp, P, Ty1});
				{true, Cs4} ->
				    {merge_types([Ty1, Ty2])
				    ,union_var_binds([VB1, VB2])
				    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])}
			    end
		    end
	    end
    end;
type_check_expr(Env, {op, _, RelOp, Arg1, Arg2}) when
      (RelOp == '=:=') or (RelOp == '==') or
      % It's debatable whether we should allow comparison between any types
      % but right now it's allowed
      (RelOp == '>=')  or (RelOp == '=<') ->
    case {type_check_expr(Env, Arg1)
	 ,type_check_expr(Env, Arg2)} of
	{{Ty1, VB1, Cs1}, {Ty2, VB2, Cs2}} ->
	    case {subtype(Ty1, Ty2, Env#env.tenv),
		  subtype(Ty2, Ty1, Env#env.tenv)} of
		{{true, Cs3}, {true, Cs4}} ->
		    % TODO: Should we return boolean() here in some cases?
		    % If both Ty1 and Ty2 are not any() then one could
		    % plausably return boolean().
		    {{type, 0, any, []}
		    ,union_var_binds([VB1, VB2])
		    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])};
		_ ->
		    throw(type_error)
	    end
    end;

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
    FieldTy = maps:get(Field, Rec),
    {VB1, Cs1} = type_check_expr_in(Env, FieldTy, Expr),
    {VB2, Cs2} = type_check_fields(Env, Rec, Fields),
    {union_var_binds([VB1, VB2]), constraints:combine(Cs1,Cs2)};
type_check_fields(_Env, _Rec, []) ->
    {#{}, constraints:empty()}.




type_check_lc(Env, Expr, []) ->
    {_Ty, _VB, Cs} = type_check_expr(Env, Expr),
    % We're returning any() here because we're in a context that doesn't
    % care what type we return. It's different for type_check_lc_in.
    {{type, 0, any, []}, #{}, Cs};
type_check_lc(Env, Expr, [{generate, _, Pat, Gen} | Quals]) ->
    {Ty,  _,  Cs1} = type_check_expr(Env, Gen),
    {TyL, VB, Cs2} = type_check_lc(Env#env{ venv = add_type_pat(Pat,
								Ty,
								Env#env.tenv,
								Env#env.venv) },
				   Expr, Quals),
    {TyL, VB, constraints:empty(Cs1,Cs2)}.




type_check_expr_in(Env, {type, _, any, []}, Expr) ->
    {_Ty, VB, Cs} = type_check_expr(Env, Expr),
    {VB, Cs};
type_check_expr_in(Env, Ty, {var, LINE, Var}) ->
    VarTy = maps:get(Var, Env#env.venv),
    case subtype(VarTy, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, tyVar, LINE, Var, VarTy, Ty})
    end;
type_check_expr_in(Env, Ty, I = {integer, LINE, Int}) ->
    case subtype(I, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, int, Int, LINE, Ty})
    end;
type_check_expr_in(Env, Ty, {float, LINE, _Int}) ->
    case subtype(Ty, {type, 0, float, []}, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, float, LINE, Ty})
    end;
type_check_expr_in(Env, Ty, Atom = {atom, LINE, _}) ->
    case subtype(Atom, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, Atom, LINE, Ty})
    end;
type_check_expr_in(Env, Ty, Cons = {cons, LINE, H, T}) ->
    case subtype({type, 0, nonempty_list, [{type, 0, any, []}]}, Ty, Env#env.tenv) of
	{true, Cs1} ->
	    {VB, Cs2} = type_check_cons_in(Env, Ty, H, T),
	    {VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, cons, LINE, Cons, Ty})
    end;
type_check_expr_in(Env, Ty, {nil, LINE}) ->
    case subtype({nil,0}, Ty, Env#env.tenv) of
	{true, Cs} ->
	    {#{}, Cs};
	false ->
	    throw({type_error, nil, LINE, Ty})
    end;
type_check_expr_in(Env, ResTy, {tuple, LINE, TS}) ->
    case subtype({type, 0, tuple, lists:duplicate(length(TS), {type, 0, any, []})}
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
type_check_expr_in(Env, ResTy, {'case', _, Expr, Clauses}) ->
    {ExprTy, VarBinds, Cs1} = type_check_expr(Env, Expr),
    Env2 = Env#env{ venv = add_var_binds(Env#env.venv, VarBinds) },
    {VB, Cs2} = check_clauses(Env2, ExprTy, ResTy, Clauses),
    {VB, constraints:combine(Cs1,Cs2)};
type_check_expr_in(Env, ResTy, {'if', _, Clauses}) ->
    check_clauses(Env, {type, 0, any, []}, ResTy, Clauses);
type_check_expr_in(Env, ResTy, {call, _, Name, Args}) ->
    {FunTy, VarBinds1, Cs} = type_check_fun(Env, Name, length(Args)),
    case FunTy of
	{type, _, any, []} ->
	    {_, VarBinds2, Css} =
		lists:unzip3([ type_check_expr(Env, Arg) || Arg <- Args]),
	    { union_var_binds([VarBinds1 |  VarBinds2])
	    , constraints:combine([Cs|Css]) };
	[{type, _, 'fun', [{type, _, product, TyArgs}, FunResTy]}] ->
	    % TODO: Handle multi-clause function types
	    {VarBinds2, Css} =
		lists:unzip([ type_check_expr_in(Env, TyArg, Arg)
			   || {TyArg, Arg} <- lists:zip(TyArgs, Args) ]),
	    case subtype(ResTy, FunResTy, Env#env.tenv) of
		{true, Cs2} ->
		    VarBind = union_var_binds([VarBinds1 | VarBinds2]),
		    {VarBind, constraints:combine([Cs, Cs2 | Css])};
		_ ->
		    throw(type_error)
	    end;
	[{type, _, bounded_fun, [{type, _, 'fun',
				  [{type, _, product, ArgTys}, FunResTy]}
				,SCs2]}] ->
	    Cs2 = constraints:convert(SCs2),
	    {VarBinds, Css} =
		lists:unzip(
		  lists:zipwith(fun (ArgTy, Arg) ->
				       type_check_expr_in(Env, ArgTy, Arg)
			       end, ArgTys, Args)
		 ),
	    case subtype(ResTy, FunResTy, Env#env.tenv) of
		{true, Cs3} ->
		    { union_var_binds([VarBinds1 | VarBinds])
		    , constraints:combine([Cs, Cs2, Cs3 | Css]) };
		false ->
		    throw(type_error)
	    end
    end;
type_check_expr_in(Env, ResTy, {'receive', _, Clauses}) ->
    check_clauses(Env, [{type, 0, any, []}], ResTy, Clauses);
type_check_expr_in(Env, ResTy, {op, _, '!', Arg1, Arg2}) ->
    % The first argument should be a pid.
    {_,  VarBinds1, Cs1} = type_check_expr(Env, Arg1),
    {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
    {union_var_binds([VarBinds1,VarBinds2]), constraints:combine(Cs1,Cs2)};
type_check_expr_in(Env, ResTy, {op, P, 'not', Arg}) ->
    case subtype({type, 0, boolean, []}, ResTy, Env#env.tenv) of
	{true, Cs1} ->
	    {VB, Cs2} = type_check_expr_in(Env, ResTy, Arg),
	    {VB, constraints:combine(Cs1, Cs2)};
	false ->
	    throw({type_error, not_user_with_wrong_type, P, ResTy})
    end;
type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '+' orelse Op == '-' orelse Op == '*' orelse Op == '/' ->
    type_check_arith_op(Env, ResTy, Op, P, Arg1, Arg2);
type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == 'bnot' orelse Op == 'div' orelse Op == 'rem' orelse
      Op == 'band' orelse Op == 'bor' orelse Op == 'bxor' orelse
      Op == 'bsl'  orelse Op == 'bsr' ->
    type_check_int_op(Env, ResTy, Op, P, Arg1, Arg2);
type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == 'and' orelse Op == 'or' orelse Op == 'xor' orelse
      Op == 'andalso' orelse Op == 'orelse' ->
    type_check_logic_op(Env, ResTy, Op, P, Arg1, Arg2);
type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '++' orelse Op == '--' ->
    type_check_list_op(Env, ResTy, Op, P, Arg1, Arg2);

type_check_expr_in(Env, ResTy, {'catch', _, Arg}) ->
    % TODO: Should we require ResTy to also include the possibility of
    % exceptions? But exceptions can be of any type! That would mean
    % that we require ResTy to be any(), or perhaps also term().
    % But that would make exceptions and types almost incompatible!
    type_check_expr_in(Env, ResTy, Arg);
type_check_expr_in(Env, ResTy, {'try', _, Block, CaseCs, CatchCs, AfterCs}) ->
    {VB,   Cs1}  = type_check_block_in(Env, ResTy, Block),
    Env2 = Env#env{ venv = add_var_binds(VB, Env#env.venv) },
    {_VB2, Cs2} = check_clauses(Env2, {type, 0, any, []}, ResTy, CaseCs),
    {_VB3, Cs3} = check_clauses(Env2, {type, 0, any, []}, ResTy, CatchCs),
    {_VB4, Cs4} = check_clauses(Env2, {type, 0, any, []}, ResTy, AfterCs),
    % TODO: Check what variable bindings actually should be propagated
    {VB
    ,constraints:combine([Cs1,Cs2,Cs3,Cs4])}.


type_check_arith_op(Env, ResTy, Op, P, Arg1, Arg2) ->
    case ResTy of
	{type, _, Ty, []} when Ty == 'integer' orelse Ty == 'float' orelse
			       Ty == 'any' ->
	  {VarBinds1, Cs1} = type_check_expr_in(Env, ResTy, Arg1),
	  {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
	  {union_var_binds([VarBinds1, VarBinds2])
	  ,constraints:combine(Cs1, Cs2)};
	_ ->
	  throw({type_error, arith_error, Op, P, ResTy})
    end.
type_check_int_op(Env, ResTy, Op, P, Arg1, Arg2) ->
    case ResTy of
	{type, _, Ty, []} when Ty == 'integer' orelse Ty == 'any' ->
	  {VarBinds1, Cs1} = type_check_expr_in(Env, ResTy, Arg1),
	  {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
	  {union_var_binds([VarBinds1, VarBinds2])
	  ,constraints:combine(Cs1, Cs2)};
	_ ->
	  throw({type_error, int_error, Op, P, ResTy})
    end.
type_check_logic_op(Env, ResTy, Op, P, Arg1, Arg2) ->
    case ResTy of
	{type, _, Ty, []} when Ty == 'boolean' orelse Ty == 'bool'
			       orelse Ty == 'any' ->
	  {VarBinds1, Cs1} = type_check_expr_in(Env, ResTy, Arg1),
	  {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
	  {union_var_binds([VarBinds1, VarBinds2])
	  ,constraints:combine(Cs1, Cs2)};
	_ ->
	  throw({type_error, logic_error, Op, P, ResTy})
    end.
type_check_list_op(Env, ResTy, Op, P, Arg1, Arg2) ->
    case ResTy of
	{type, _, 'list', [_Ty]} ->
	  {VarBinds1, Cs1} = type_check_expr_in(Env, ResTy, Arg1),
	  {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
	  {union_var_binds([VarBinds1, VarBinds2])
	  ,constraints:combine(Cs1, Cs2)};
	{type, _, any, []} ->
	  {VarBinds1, Cs1} = type_check_expr_in(Env, ResTy, Arg1),
	  {VarBinds2, Cs2} = type_check_expr_in(Env, ResTy, Arg2),
	  {union_var_binds([VarBinds1, VarBinds2])
	  ,constraints:combine(Cs1, Cs2)};
	_ ->
	  throw({type_error, list_op_error, Op, P, ResTy})
    end.


type_check_assocs(Env, [{Assoc, _, Key, Val}| Assocs])
  when Assoc == map_field_assoc orelse Assoc == map_field_exact ->
    {_KeyTy, _KeyVB, Cs1} = type_check_expr(Env, Key),
    {_ValTy, _ValVB, Cs2} = type_check_expr(Env, Val),
    % TODO
    {Ty, VB, Cs} = type_check_assocs(Env, Assocs),
    {Ty, VB, constraints:combine([Cs, Cs1, Cs2])};
type_check_assocs(_Env, []) ->
    {{type, 0, any, []}, #{}, constraints:empty()}.


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
			type_check_expr_in(Env, {type, 0, any, []}, Expr)
		end, TS)),
    {union_var_binds(VBs), constraints:combine(Css)};
type_check_tuple_in(Env, {type, _, tuple, Tys}, TS) ->
    {VBs, Css} = lists:unzip(
      lists:zipwith(fun (Ty, Expr) ->
			type_check_expr_in(Env, Ty, Expr)
		end, Tys, TS)),
    {union_var_binds(VBs), constraints:combine(Css)};
type_check_tuple_in(Env, {type, _, union, Tys}, TS) ->
    type_check_tuple_union(Env, Tys, TS).

type_check_tuple_union(Env, [Tuple = {type, _, tuple, _}|Union], TS) ->
    try type_check_tuple_in(Env, Tuple, TS)
    catch
	_ ->
	    type_check_tuple_union(Env, Union, TS)
    end;
type_check_tuple_union(Env, [_|Union], TS) ->
    type_check_tuple_union(Env, Union, TS);
type_check_tuple_union(_Env, [], _TS) ->
    %% TODO: Better error message
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
    type_check_cons_union(Env, Tys, H, T).

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
    {Tys, VarBinds, Css} =
	lists:unzip3(lists:map(fun (Clause) ->
				       infer_clause(Env, Clause)
			       end, Clauses)),
    {merge_types(Tys), union_var_binds(VarBinds), constraints:combine(Css)}.

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


check_clauses(Env, ArgsTy, ResTy, Clauses) when
      not is_list(ArgsTy) ->
    check_clauses(Env, [ArgsTy], ResTy, Clauses);
check_clauses(Env, ArgsTy, ResTy, Clauses) ->
    {VarBinds, Css} =
	lists:unzip(lists:map(fun (Clause) ->
				  check_clause(Env, ArgsTy, ResTy, Clause)
			  end, Clauses)),
    {VarBinds, constraints:combine(Css)}.

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
					    begin
						{_Ty, VB} = type_check_expr(Env, Guard), % Do we need to thread the Env?
						VB
					    end
				    end, GuardSeq))
		end, Guards)).

type_check_function(FEnv, TEnv, {function,_, Name, NArgs, Clauses}) ->
    case maps:find({Name, NArgs}, FEnv) of
	{ok, [{type, _, 'fun', [{type, _, product, ArgsTy}, ResTy]}]} ->
	    % TODO: Handle multi-clause function types
	    {VarBinds, Cs} = check_clauses(#env{ fenv = FEnv, tenv = TEnv },
					   ArgsTy, ResTy, Clauses),
	    {ResTy, VarBinds, Cs};
	{ok, {type, _, any, []}} ->
	    infer_clauses(#env{ fenv = FEnv, tenv = TEnv }, Clauses);
	error ->
	    throw({internal_error, missing_type_spec, Name, NArgs})
    end.

merge_types([]) ->
    {type, 0, any, []};
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
		    merge_types([{type, 0, boolean, []} | Rest]);
		[{atom, _, true}, {atom, _, false} | Rest] ->
		    merge_types([{type, 0, boolean, []} | Rest]);
		[{atom, _, _}, {type, _, _, _} | _] ->
		    {type, 0, any, []};
		[{type, P, Ty, Args1}, {type, _, Ty, Args2}]
		  when length(Args1) == length(Args2) ->
		    {type, P, Ty, lists:zipwith(fun (A,B) -> merge_types([A,B])
						end, Args1, Args2)};
		[{type, P, tuple, Args1}, {type, _, tuple, Args2} | Rest] ->
		    case length(Args1) == length(Args2) of
			false ->
			    {type, 0, any, []};
			true  ->
			    merge_types([{type, P, tuple,
					  lists:zipwith(fun (A1, A2) ->
								merge_types([A1,A2]) end,
							Args1, Args2)}
					 | Rest])
		    end;
		[{type, _, map, Assocs}, {type, _, map, Assocs}] ->
		    % TODO: Figure out how to merge field assocs properly
		    [{type, 0, map, []}]
	    end
    end.

add_types_pats([], [], _TEnv, VEnv) ->
    VEnv;
add_types_pats([Pat | Pats], [Ty | Tys], TEnv, VEnv) ->
    add_types_pats(Pats, Tys, TEnv, add_type_pat(Pat, Ty, TEnv, VEnv)).

add_type_pat({var, _, '_'}, _Ty, _TEnv, VEnv) ->
    VEnv;
add_type_pat({var, _, A}, Ty, _TEnv, VEnv) ->
    VEnv#{ A => Ty };
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
    case subtype(Nil, Ty, TEnv) of
	% There cannot be any constraints generated in this case
	{true, _Cs} ->
	    VEnv;
	false ->
	    throw({type_error, pattern, P, Nil, Ty})
    end;
add_type_pat({cons, _, PH, PT}, ListTy = {type, _, list, [ElemTy]}, TEnv, VEnv) ->
    VEnv2 = add_type_pat(PH, ElemTy, TEnv, VEnv),
    add_type_pat(PT, ListTy, TEnv, VEnv2);
add_type_pat({cons, _, PH, PT}, ListTy = {type, _, list, []}, TEnv, VEnv) ->
    VEnv2 = add_any_types_pat(PH, VEnv),
    add_type_pat(PT, ListTy, TEnv, VEnv2);
add_type_pat({record, _, _Record, Fields}, {type, _, record, [{atom, _, _RecordName}]}, TEnv, VEnv) ->
    % TODO: We need the definitions of records here, to be able to add the
    % types of the matches in the record.
    add_type_pat_fields(Fields, {type, 0, any, []}, TEnv, VEnv);
add_type_pat({match, _, Pat1, Pat2}, Ty, TEnv, VEnv) ->
    add_type_pat(Pat1, Ty, TEnv, add_type_pat(Pat2, Ty, TEnv, VEnv));

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

add_type_pat_tuple(Pats, {type, _, tuple, any}, _TEnv, VEnv) ->
    add_any_types_pats(Pats, VEnv);
add_type_pat_tuple(Pats, {type, _, tuple, Tys}, TEnv, VEnv) ->
    add_types_pats(Pats, Tys, TEnv, VEnv);
add_type_pat_tuple(Pats, {type, _, union, Tys}, TEnv, VEnv) ->
%% TODO: This code approximates unions of tuples with tuples of unions
    Unions =
	lists:map(fun (UnionTys) ->
			  {type, 0, union, UnionTys}
		  end
		 ,transpose([TS
			   || {type, _, tuple, TS} <- Tys
			    , length(TS) == length(Pats)])),
    lists:foldl(fun ({Pat, Union}, Env) ->
			add_type_pat(Pat, Union, TEnv, Env)
		end, VEnv, lists:zip(Pats, Unions)).


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
add_any_types_pat({nil, _}, VEnv) ->
    VEnv;
add_any_types_pat({tuple, _, Pats}, VEnv) ->
    add_any_types_pats(Pats, VEnv);
add_any_types_pat({var, _,'_'}, VEnv) ->
    VEnv;
add_any_types_pat({var, _,A}, VEnv) ->
    VEnv#{ A => {type, 0, any, []} }.

%%% Helper functions

return(X) ->
    { X, #{}, constraints:empty() }.

union_var_binds([]) ->
    #{};
union_var_binds([ VarBind | VarBinds ]) ->
    merge(fun glb_types/2, VarBind, union_var_binds(VarBinds)).

add_var_binds(VEnv, VarBinds) ->
    merge(fun glb_types/2, VEnv, VarBinds).

merge(F, M1, M2) ->
    maps:fold(fun (K, V1, M) ->
		 maps:update_with(K, fun (V2) -> F(V1, V2) end, V1, M)
	 end, M2, M1).

% TODO: improve
% Is this the right function to use or should I always just return any()?
glb_types({type, _, N, Args1}, {type, _, N, Args2}) ->
    Args = [ glb_types(Arg1, Arg2) || {Arg1, Arg2} <- lists:zip(Args1, Args2) ],
    {type, 0, N, Args};
glb_types(_, _) ->
    {type, 0, any, []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


type_check_file(File) ->
    case gradualizer_db:start_link() of
	{ok, _Pid}                    -> ok;
	{error, {already_started, _}} -> ok
    end,
    {ok, Forms} = epp:parse_file(File,[]),
    #parsedata{specs     = Specs
	      ,functions = Funs
	      ,types     = Types
	      ,opaques   = Opaques
	      ,records   = Records
	      } =
	collect_specs_types_opaques_and_functions(Forms),
    FEnv = create_fenv(Specs, Funs),
    TEnv = create_tenv(Types ++ Opaques, Records),
    lists:foldr(fun (Function, ok) ->
			try type_check_function(FEnv, TEnv, Function) of
			    {_Ty, _VarBinds, _Cs} ->
				ok
			catch
			    Throw ->
				% Useful for debugging
				% io:format("~p~n", [erlang:get_stacktrace()]),
				handle_type_error(Throw),
				nok
			end;
		    (_Function, Err) ->
			Err
		end, ok, Funs).

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
    maps:from_list([ {{Name, NArgs}, {type, 0, any, []}}
		     || {function,_, Name, NArgs, _Clauses} <- Funs
		   ] ++
		   [ {{Name, NArgs}, Types} || {{Name, NArgs}, Types} <- Specs
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
handle_type_error({type_error, tyVar, LINE, Var, VarTy, Ty}) ->
    io:format("The variable ~p on line ~p has type ~s "
	      "but is expected to have type ~s~n",
	      [Var, LINE, typelib:pp_type(VarTy), typelib:pp_type(Ty)]);
handle_type_error({type_error, {atom, _, A}, LINE, Ty}) ->
    io:format("The atom ~p on line ~p does not have type ~s~n",
	      [A, LINE, typelib:pp_type(Ty)]);
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
handle_type_error({type_error, call, _P, Name, TyArgs, ArgTys}) ->
    io:format("The function ~p expects arguments of type~n~p~n but is given "
	      "arguments of type~n~p~n",
	      [Name, TyArgs, ArgTys]);
handle_type_error({type_error, boolop, BoolOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-boolean argument "
	      " of type ~s~n", [BoolOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, arith_error, ArithOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-numeric argument "
	      " of type ~s~n", [ArithOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, int_error, IntOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-integer argument "
	      " of type ~s~n", [IntOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, logic_error, LogicOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-boolean argument "
	      " of type ~s~n", [LogicOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, list_op_error, ListOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-list argument "
	      " of type ~s~n", [ListOp, P, typelib:pp_type(Ty)]);
handle_type_error({type_error, tuple_error}) ->
    io:format("A tuple didn't match any of the types in a union~n");
handle_type_error({type_error, pattern, P, Pat, Ty}) ->
    io:format("The pattern ~s on line ~p doesn't have the type ~s~n",
	      [erl_pp:expr(Pat), P, typelib:pp_type(Ty)]);
handle_type_error({unknown_variable, P, Var}) ->
    io:format("Unknown variable ~p on line ~p.~n", [Var, P]);
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
	    paux(List, Fun, erlang:setelement(I, [Item | erlang:element(I,Tuple)], Tuple));
	false ->
	    paux(List, Fun, Tuple)
    end.
