%% @private
%% @doc
%% Functions operating on types represented as the Erlang Abstract Forms.
%% @end
-module(typelib).

-export([remove_pos/1,
         remove_pos_all/1,
         annotate_user_type/2, annotate_user_types/2,
         get_module_from_annotation/1,
         substitute_type_vars/2,
         pp_type/1, debug_type/3, parse_type/1,
         reduce_type/3]).
-export_type([constraint/0, function_type/0, printable_type/0]).

-type anno() :: erl_anno:anno().
-type type() :: gradualizer_type:abstract_type().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing and pretty printing types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("stdlib/include/assert.hrl").
-include("gradualizer.hrl").

-type constraint() :: {type, erl_anno:anno(),
                             constraint,
                             {atom, erl_anno:anno(), is_subtype},
                             [{var, erl_anno:anno(), atom()} | type()]}.
-type function_type() :: {type, erl_anno:anno(),
                                'fun',
                                [{type, erl_anno:anno(), product, [type()]} |
                                 type()]}.
-type bounded_fun() :: {type, erl_anno:anno(), bounded_fun,
                                 [function_type() | [constraint()]]}.
-type printable_type() :: type() | bounded_fun().

%% @doc
%% Pretty-print a type represented as an Erlang abstract form.
%% @end
-spec pp_type(printable_type() | [printable_type()]) -> io_lib:chars().
pp_type(Types = [_|_]) ->
    %% TODO: This is a workaround for the fact that a list is sometimes used in
    %% place of a type. It typically represents a function type with multiple clauses.
    %% We should perhaps represent them as a tuples of the form
    %% {type, Anno, intersection, Types} instead.
    lists:join("; ", lists:map(fun pp_type/1, Types));
pp_type({type, _, bounded_fun, [FunType, []]}) ->
    %% Bounded fun with empty constraints gets printed with a trailing "when"
    %% when pretty-printed as a spec (next clause)
    pp_type(?assert_type(FunType, function_type()));
pp_type(Type = {type, _, bounded_fun, _}) ->
    %% erl_pp can't handle bounded_fun in type definitions
    %% We invent our own syntax here, e.g. "fun((A) -> ok when A :: atom())"
    TypePp = type_for_erl_pp(Type),
    Form = {attribute, erl_anno:new(0), spec, {{foo, 0}, [TypePp]}},
    TypeDef = erl_pp:form(Form),
    {match, [S]} = re:run(TypeDef, <<"-spec foo\\s*(.*)\\.\\n*$">>,
                          [{capture, all_but_first, list}, dotall]),
    ?assert(is_list(S), regex_match_not_a_string),
    S = ?assert_type(S, string()),
    "fun(" ++ S ++ ")";
pp_type({var, _, TyVar}) ->
    %% See gradualizer_type:af_type_variable/0 and typechecker:new_type_var/0
    if
        is_atom(TyVar) -> atom_to_list(TyVar);
        is_list(TyVar) -> TyVar
    end;
pp_type([]) ->
    error({badarg, []});
pp_type(Type) ->
    %% erl_pp can handle type definitions, so wrap Type in a type definition
    %% and then take the type from that.
    TypePp = type_for_erl_pp(Type),
    Form = {attribute, erl_anno:new(0), type, {t, TypePp, []}},
    TypeDef = erl_pp:form(Form),
    {match, [S]} = re:run(TypeDef, <<"::\\s*(.*)\\.\\n*">>,
                          [{capture, all_but_first, list}, dotall]),
    ?assert(is_list(S), regex_match_not_a_string),
    S = ?assert_type(S, string()),
    case S of
        "INVALID" ++ _ -> error({badarg, Type});
        _ -> ok end,
    S.
    %case erl_anno:file(element(2, Type)) of
    %        undefined -> S;
    %        File      -> S ++ " in " ++ File
    %end.


%% Transform our `type()' into the standard `erl_parse:abstract_type()' that erl_pp accepts.
-spec type_for_erl_pp(type()) -> type();
                     (bounded_fun()) -> bounded_fun().
type_for_erl_pp({rigid_var, P, Var}) ->
    {var, P, Var};
type_for_erl_pp({type, P, Tag, Args}) when is_list(Args) ->
    {type, P, Tag, [type_for_erl_pp(Arg) || Arg <- Args]};
type_for_erl_pp({user_type, P, Tag, Args}) ->
    {user_type, P, Tag, [type_for_erl_pp(Arg) || Arg <- Args]};
type_for_erl_pp({remote_type, P, [Mod, Fun, Args]}) ->
    {remote_type, P, [Mod, Fun, [type_for_erl_pp(Arg) || Arg <- Args]]};
type_for_erl_pp(Type) ->
    Type.

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
%% Annotated types as in Name :: Type are also removed.
-spec remove_pos(any()) -> any().
remove_pos({type, _, any}) ->
    {type, erl_anno:new(0), any};
remove_pos({type, _, constraint, [{atom, _, is_subtype}, Args]}) ->
    Args = ?assert_type(Args, [type()]),
    L = erl_anno:new(0),
    {type, L, constraint, [{atom, L, is_subtype}, lists:map(fun remove_pos/1, Args)]};
remove_pos({Type, _, Value})
  when Type == atom; Type == integer; Type == char; Type == var; Type == rigid_var ->
    {Type, erl_anno:new(0), Value};
remove_pos({user_type, Anno, Name, Params}) when is_list(Params) ->
    {user_type, anno_keep_only_filename(Anno), Name,
     lists:map(fun remove_pos/1, Params)};
remove_pos({type, Anno, record, [Name | TypedFields]}) ->
    {type, anno_keep_only_filename(Anno), record,
     [remove_pos(Name)] ++ lists:map(fun remove_pos/1, TypedFields)};
remove_pos({type, _, field_type, [FName, FTy]}) ->
    {type, erl_anno:new(0), field_type, [remove_pos(FName), remove_pos(FTy)]};
remove_pos({type, _, Type, Params}) when is_list(Params) ->
    {type, erl_anno:new(0), Type, lists:map(fun
                                                (P) when is_list(P) -> remove_pos_all(P);
                                                (P) -> remove_pos(P)
                                            end, Params)};
remove_pos({type, _, Type, any}) when Type == tuple; Type == map ->
    {type, erl_anno:new(0), Type, any};
remove_pos({type, _, Assoc, Tys})
  when Assoc == map_field_exact;
       Assoc == map_field_assoc ->
    {type, erl_anno:new(0), Assoc, lists:map(fun remove_pos/1, Tys)};
remove_pos({remote_type, _, [Mod, Name, Params]}) ->
    Params = ?assert_type(Params, list()),
    Params1 = lists:map(fun remove_pos/1, Params),
    {remote_type, erl_anno:new(0), [Mod, Name, Params1]};
remove_pos({ann_type, _, [_Var, Type]}) ->
    %% Also remove annotated types one the form Name :: Type
    remove_pos(?assert_type(Type, type()));
remove_pos({op, _, Op, Type}) ->
    {op, erl_anno:new(0), Op, remove_pos(Type)};
remove_pos({op, _, Op, Type1, Type2}) ->
    {op, erl_anno:new(0), Op, remove_pos(Type1), remove_pos(Type2)}.

-spec remove_pos_all(list()) -> list().
remove_pos_all([]) ->
    [];
remove_pos_all([_|_] = L) ->
    lists:map(fun remove_pos/1, L).

%% Helper for remove_pos/1. Removes all annotations except filename.
-spec anno_keep_only_filename(erl_anno:anno()) -> erl_anno:anno().
anno_keep_only_filename(Anno) ->
    NewAnno = erl_anno:new(0),
    case erl_anno:file(Anno) of
        undefined -> NewAnno;
        Filename  -> erl_anno:set_file(Filename, NewAnno)
    end.

-type mod_or_file() :: module() | file:filename().

%% Annotate a user-defined type or record type with a file name.
-spec annotate_user_type(mod_or_file(), type()) -> type().
annotate_user_type(ModOrFile, Type) ->
    Filename = ensure_filename(ModOrFile),
    annotate_user_type_(Filename, Type).

-spec ensure_filename(module() | file:filename()) -> file:filename().
ensure_filename(ModOrFile) ->
    case ModOrFile of
        Module when is_atom(ModOrFile) ->
            atom_to_list(?assert_type(Module, atom())) ++ ".erl";
        _ -> ModOrFile
    end.

%% Annotate user-defined types and record types with a file name.
%%
%% Please note that once we recurse down the gradualizer_type:abstract_type() tree,
%% the lists of type arguments might contain types, but might also contain other non-type nodes.
%% For a top-level type() we guarantee to return a type(),
%% but for an inner node we'll return that node unchanged.
-spec annotate_user_types(mod_or_file(), type()) -> type();
                         (mod_or_file(), list()) -> list().
annotate_user_types(_ModOrFile, []) -> [];
annotate_user_types(ModOrFile, Types = [_|_]) ->
    [ annotate_user_type(ModOrFile, Type) || Type <- ?assert_type(Types, [type()]) ];
annotate_user_types(ModOrFile, Type) ->
    annotate_user_type(ModOrFile, ?assert_type(Type, type())).

%% @see annotate_user_types/2
-spec annotate_user_type_(file:filename(), any()) -> any().
annotate_user_type_(Filename, {user_type, Anno, Name, Params}) ->
    %% Annotate local user-defined type.
    {user_type, erl_anno:set_file(Filename, Anno), Name,
     [annotate_user_type_(Filename, Param) || Param <- Params]};
annotate_user_type_(Filename, {type, Anno, record, RecName = [_]}) ->
    RecName = ?assert_type(RecName, [gradualizer_type:af_atom(), ...]),
    %% Annotate local record type
    {type, erl_anno:set_file(Filename, Anno), record, RecName};
annotate_user_type_(Filename, {type, Anno, T, Params}) when is_list(Params) ->
    {type, Anno, T, [ annotate_user_types(Filename, Param) || Param <- Params ]};
annotate_user_type_(Filename, {ann_type, Anno, [Var, Type]}) ->
    %% We match Var :: af_anno() and Type :: type() above.
    Type = ?assert_type(Type, type()),
    {ann_type, Anno, [Var, annotate_user_type_(Filename, Type)]};
annotate_user_type_(_Filename, Type) ->
    Type.

-spec get_module_from_annotation(erl_anno:anno()) -> {ok, module()} | none.
get_module_from_annotation(Anno) ->
    case erl_anno:file(Anno) of
        undefined ->
            none;
        File ->
            case unicode:characters_to_binary(filename:basename(File, ".erl")) of
                Basename when is_binary(Basename) ->
                    {ok, binary_to_existing_atom(Basename)};
                _ ->
                    erlang:error({malformed_anno, Anno})
            end
    end.

-spec substitute_type_vars(type(),
                           #{atom() => type()}) -> type().
substitute_type_vars({type, L, 'fun', [Any = {type, _, any}, RetTy]}, TVars) ->
    %% Special case for `fun((...) -> R)',
    %% the only place where `{type, _, any}' can occur.
    %% We match on `{type, _, any}' in the head explicitly, so `RetTy' cannot contain it - the
    %% assertion is safe.
    RetTy = ?assert_type(RetTy, type()),
    {type, L, 'fun', [Any, substitute_type_vars(RetTy, TVars)]};
substitute_type_vars({Tag, L, T, Params}, TVars)
  when Tag == type orelse
       Tag == user_type,
       is_list(Params) ->
    type(Tag, L, T, [substitute_type_vars(P, TVars) || P <- Params]);
substitute_type_vars({remote_type, L, [M, T, Params]}, TVars) ->
    {remote_type, L, [M, T, [substitute_type_vars(P, TVars) || P <- Params]]};
substitute_type_vars({ann_type, L, [Var = {var, _, _}, Type]}, TVars) ->
    %% We matched out Var :: af_anno() from [af_anno() | type()] above.
    Type = ?assert_type(Type, type()),
    {ann_type, L, [Var, substitute_type_vars(Type, TVars)]};
substitute_type_vars({var, L, Var}, TVars) ->
    case TVars of
        #{Var := Type} -> Type;
        _              -> {var, L, Var}
    end;
substitute_type_vars({rigid_var, L, Var}, TVars) when is_atom(Var) ->
    case TVars of
        #{Var := Type} -> Type;
        _              -> {var, L, Var}
    end;
substitute_type_vars(Other = {type, _, T, any}, _)
  when T == tuple; T == map ->
    Other;
substitute_type_vars(Other = {op, _, _Op, _Arg}, _) ->
    %% unary integer operator - cannot contain type vars
    Other;
substitute_type_vars(Other = {op, _, _Op, _Arg1, _Arg2}, _) ->
    %% binary integer operator - cannot contain type vars
    Other;
substitute_type_vars(Other = {T, _, _}, _)
  when T == atom; T == integer; T == char ->
    Other.

-spec type(any(), anno(), atom(), [type()]) -> type().
type(Tag, L, TyName, Params) when Tag =:= type; Tag =:= user_type ->
    {Tag, L, TyName, Params}.

-type walkable_type() :: gradualizer_type:abstract_type() | {type, _, any} | pos_inf | neg_inf.
%% `gradualizer_type:abstract_type()' defines the abstract representation of a type.
%% The type is a tree of nodes. However, there are more node kinds in the tree,
%% than might appear at the top-level (as the root node).
%% In order to specify a function which can traverse all nodes, not just the top-level nodes,
%% we have to include all possible node kinds in the type definition.

%% @doc `reduce_type/3' enables reducing an abstract type to a single value.
%%
%% Example 1 - gather all singleton atoms occurring in a type:
%%
%% ```
%% > F = fun
%% >         ({atom, _, _} = At, Acc) -> {At, [At | Acc]};
%% >         (Ty, Acc) -> {Ty, Acc}
%% >     end,
%% > {_, [{atom, _, my_atom}]} = typelib:reduce_type(F, [], typelib:parse_type("A :: {my_atom}")).
%% '''
%%
%% `Fun' can skip traversing parts of the type tree by matching on it
%% and returning `none()' instead of the actual subtree.
%%
%% Example 2 - gather singleton atoms, but skip a particular branch of a union type:
%%
%% ```
%% > ComplexTy = typelib:parse_type("atom1 | atom2 | "
%% >                                "{complex, integer(), [{atom() | string(), number()}]}"),
%% > F = fun
%% >          ({type, _, tuple, [{atom, _, complex} | _]}, Acc) -> { {type, 0, none, []}, Acc };
%% >          ({atom, _, Name} = Ty, Acc) -> {Ty, [Name | Acc]};
%% >          (Ty, Acc) -> {Ty, Acc}
%% >      end,
%% > {_, [atom2, atom1]} = reduce(F, [], ComplexTy).
%% '''
%% @end
%% TODO: this is mostly redundant with `gradualizer_lib:fold_ast'
-spec reduce_type(Fun, Acc, walkable_type()) -> R when
      Fun :: fun((walkable_type(), Acc) -> {walkable_type(), Acc}),
      R :: {walkable_type(), Acc}.
reduce_type(Fun, Acc, Type) ->
    reduce(Fun, apply, Acc, Type).

-spec reduce(Fun, Action, Acc, walkable_type()) -> R when
      Fun :: fun((walkable_type(), Acc) -> {walkable_type(), Acc}),
      Action :: apply | recurse,
      R :: {walkable_type(), Acc}.
reduce(Fun, _, Acc, {'atom', _, _} = Ty)              -> Fun(Ty, Acc);
reduce(Fun, _, Acc, {'type', _Anno, _Name, any} = Ty) -> Fun(Ty, Acc);
reduce(Fun, _, Acc, {'integer', _, _} = Ty)           -> Fun(Ty, Acc);
reduce(Fun, _, Acc, {'char', _, _} = Ty)              -> Fun(Ty, Acc);
reduce(Fun, _, Acc, {'type', _Anno, any} = Ty)        -> Fun(Ty, Acc);
reduce(Fun, _, Acc, pos_inf = Ty)                     -> Fun(Ty, Acc);
reduce(Fun, _, Acc, neg_inf = Ty)                     -> Fun(Ty, Acc);
reduce(Fun, _, Acc, {var, _, _} = Ty)                 -> Fun(Ty, Acc);
reduce(Fun, _, Acc, {rigid_var, _, _} = Ty)           -> Fun(Ty, Acc);
reduce(Fun, apply, Acc, Ty) ->
    {NewTy, Acc1} = Fun(Ty, Acc),
    reduce(Fun, recurse, Acc1, NewTy);
reduce(Fun, recurse, Acc, {'op', _, _, Ty1})                  -> reduce_rec(Fun, Acc, [Ty1]);
reduce(Fun, recurse, Acc, {'op', _, _, Ty1, Ty2})             -> reduce_rec(Fun, Acc, [Ty1, Ty2]);
reduce(Fun, recurse, Acc, {'ann_type', _Anno, Args})          -> reduce_rec(Fun, Acc, Args);
reduce(Fun, recurse, Acc, {'type', _Anno, _Name, Args})       -> reduce_rec(Fun, Acc, Args);
reduce(Fun, recurse, Acc, {'remote_type', _Anno, [M, T, As]}) -> reduce_rec(Fun, Acc, [M, T | As]);
reduce(Fun, recurse, Acc, {'user_type', _Anno, _Name, Args})  -> reduce_rec(Fun, Acc, Args).

reduce_rec(Fun, Acc, Args) ->
    lists:foldl(fun (Arg, {_, Acc1}) ->
                        reduce(Fun, apply, Acc1, Arg)
                end, {ok, Acc}, Args).
