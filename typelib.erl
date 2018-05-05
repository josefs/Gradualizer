%% @doc Functions operating on types on the Erlang Abstract Form
-module(typelib).

-export([remove_pos/1,
         pp_type/1, debug_type/3, parse_type/1]).

-type type() :: erl_parse:abstract_type().

%% A type used while normalizing integer types. The ranges that are possible to
%% write in the type system, i.e. non_neg_integer(), pos_integer(),
%% neg_integer(), integer(), ranges, singleton integers and unions of these.
-type int_range() :: {neg_inf, -1 | non_neg_integer() | pos_inf} |
                     {neg_integer() | 0 | 1, pos_inf} |
                     {integer(), integer()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing and pretty printing types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec pp_type(type()) -> string().
pp_type(Type) ->
    %% erl_pp can handle type definitions, so wrap Type in a type definition
    %% and then take the type from that.
    Form = {attribute, 0, type, {t, Type, []}},
    TypeDef = erl_pp:form(Form),
    {match, [S]} = re:run(TypeDef, <<"::\\s*(.*)\\.\\n*">>,
			  [{capture, all_but_first, list}, dotall]),
    S.
    %case erl_anno:file(element(2, Type)) of
    %	undefined -> S;
    %	File      -> S ++ " in " ++ File
    %end.

%% Looks up and prints the type M:N(P1, ..., Pn).
debug_type(M, N, P) ->
    case gradualizer_db:get_type(M, N, P) of
	{ok, T} ->
	    Params = lists:join($,, lists:map(fun pp_type/1, P)),
	    io:format("~w:~w(~s) :: ~s.~n",
		      [M, N, Params, pp_type(T)]);
	not_found ->
	    not_found
    end.

-spec parse_type(string()) -> type().
parse_type(Src) ->
    AttrSrc = "-type t() :: " ++ Src ++ ".",
    {ok, Tokens, _EndLocation} = erl_scan:string(AttrSrc),
    {ok, {attribute, _, type, {t, Type, []}}} = erl_parse:parse_form(Tokens),
    Type.

%% Removes all annotations from type, except filename in two cases: Filename is
%% kept for user-defined types and record types. Filename is used to
%% disambiguate between types with the same name from different modules.
-spec remove_pos(type()) -> type().
remove_pos({Type, _, Value}) when Type == atom; Type == integer; Type == var ->
    {Type, erl_anno:new(0), Value};
remove_pos({user_type, Anno, Name, Params}) when is_list(Params) ->
    {user_type, anno_keep_only_filename(Anno), Name,
     lists:map(fun remove_pos/1, Params)};
remove_pos({type, Anno, record, Params = [_Name]}) ->
    {type, anno_keep_only_filename(Anno), record, Params};
remove_pos({type, _, Type, Params}) when is_list(Params) ->
    {type, erl_anno:new(0), Type, lists:map(fun remove_pos/1, Params)};
remove_pos({type, _, Type, any}) when Type == tuple; Type == map ->
    {type, erl_anno:new(0), Type, any};
remove_pos({remote_type, _, [Mod, Name, Params]}) ->
    {remote_type, erl_anno:new(0), [Mod, Name, remove_pos(Params)]};
remove_pos({ann_type, _, [Var, Type]}) ->
    {ann_type, erl_anno:new(0), [Var, remove_pos(Type)]}.

%% Helper for remove_pos/1. Removes all annotations except filename.
-spec anno_keep_only_filename(erl_anno:anno()) -> erl_anno:anno().
anno_keep_only_filename(Anno) ->
    NewAnno = erl_anno:new(0),
    case erl_anno:file(Anno) of
	undefined -> NewAnno;
	Filename  -> erl_anno:set_file(Filename, NewAnno)
    end.


%% Weak head normalization of types
%%
%% * Replace type synonyms of built-in types
%% * Normalize union
%%
%% Normalize union
%% ---------------
%%
%% * Flatten, involves unfolding user-defined and remote types whenever they
%%   expand to unions
%% * Remove subtypes of other types in the same union; keeping any() separate
%% * Merge integer types, including singleton integers and ranges
%%   1, 1..5, integer(), non_neg_integer(), pos_integer(), neg_integer()

%% Replace built-in type aliases with their types, non-recursive
-spec expand_builtin_aliases(type()) -> type().
%expand_builtin_aliases({var, Ann, '_'}) ->
%    {type, Ann, any, []};
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


-spec normalize(type(), map()) -> type().
normalize({type, _, union, Types}, TEnv) ->
    Ts1 = flatten_unions(Types, TEnv),
    case merge_union_types(Ts1, TEnv) of
        []  -> {type, 0, none, []};
        [T] -> T;
        Ts  -> {type, 0, union, Ts}
    end.

merge_union_types(Types, TEnv) ->
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
                remove_subtypes(OtherTypes, TEnv) ++
                remove_subtypes(Anys, TEnv)
    end.

%% Keep only types that are not subtypes of any other types in the list.
remove_subtypes(Types, TEnv) -> remove_subtypes_help(Types, Types, TEnv).

remove_subtypes_help([T|Ts], Types, TEnv) ->
    case any_type(T, Types -- [T], TEnv) of
        true -> remove_subtypes_help(Ts, Types, TEnv);
        false -> [T | remove_subtypes_help(Ts, Types, TEnv)]
    end.

%% stub for testing FIXME
any_type(_, _, _) -> false.

-spec is_int_type(type()) -> boolean().
is_int_type({type, _, T, _})
  when T == pos_integer; T == non_neg_integer; T == neg_integer;
       T == integer; T == range -> true;
is_int_type({integer, _, _}) -> true;
is_int_type(_) -> false.


%% Merges integer types by sorting on the lower bound and then merging adjacent
%% ranges. Returns a list of mutually exclusive integer types.
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


%% Merges ranges. Returns a list of mutually exclusive ranges.
%% DELETEME
-spec merge_ranges([{integer(), integer()}]) -> [{integer(), integer()}].
merge_ranges([]) ->
    [];
merge_ranges(Ranges) ->
    [R | Rs] = lists:sort(Ranges),
    merge_ranges_help(Rs, [R]).

merge_ranges_help([{R1, R2} | Rs], [{S1, S2} | Stack]) when R1 =< S2 + 1 ->
    %% Overlapping or adjacent
    merge_ranges_help(Rs, [{S1, max(R2, S2)} | Stack]);
merge_ranges_help([R|Rs], Stack) ->
    merge_ranges_help(Rs, [R|Stack]);
merge_ranges_help([], Stack) ->
    lists:reverse(Stack).

%% Expands nested unions. Returs a list of types to include in the flat union.
%% Expands user-defined and remote types, only one level unless they expand to
%% unions or other user-defined or remote types.
-spec flatten_unions([type()], map()) -> [type()].
flatten_unions([{type, _, union, U} | Ts], TEnv) ->
    flatten_unions(U ++ Ts, TEnv);
flatten_unions([{user_type, _, Name, Args} | Ts], TEnv) ->
    T = unfold_user_type(Name, Args, TEnv),
    flatten_unions([T | Ts], TEnv);
flatten_unions([{remote_type, _P, Module, Name, Args} | Ts], TEnv) ->
    case gradualizer_db:get_exported_type(Module, Name, Args) of
        {ok, T} ->
            flatten_unions([T | Ts], TEnv);
        not_found ->
            throw({undef, remote_type, {Module, Name, length(Args)}})
    end;
flatten_unions([T | Ts], TEnv) ->
    [T | flatten_unions(Ts, TEnv)].

unfold_user_type(Name, Args, TEnv) ->
    Key = {Name, length(Args)},
    case TEnv of
        #{Key := T} ->
            substitute_type_vars(T, Args);
        _ ->
            throw({undef, user_type, {Name, length(Args)}})
    end.

-spec substitute_type_vars(type(),
                           #{atom() => type()}) -> type().
substitute_type_vars({type, L, T, Params}, TVars) when is_list(Params) ->
    {type, L, T, [substitute_type_vars(P, TVars) || P <- Params]};
substitute_type_vars({var, L, Var}, TVars) ->
    case TVars of
        #{Var := Type} -> Type;
        _              -> {var, L, Var}
    end;
substitute_type_vars(Other, _) ->
    Other.

