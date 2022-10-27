%% @doc Functions operating on types on the Erlang Abstract Form
-module(typelib).

-export([remove_pos/1,
         annotate_user_type/2, annotate_user_types/2,
         get_module_from_annotation/1,
         substitute_type_vars/2,
         pp_type/1, debug_type/3, parse_type/1,
         reduce_type/3]).
-export_type([constraint/0, function_type/0, extended_type/0]).

-type af_constraint() :: gradualizer_type:af_constraint().
-type type() :: gradualizer_type:abstract_type().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing and pretty printing types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("gradualizer.hrl").

-type constraint() :: {type, erl_anno:anno(),
                             constraint,
                             {atom, erl_anno:anno(), is_subtype},
                             [{var, erl_anno:anno(), atom()} | type()]}.
-type function_type() :: {type, erl_anno:anno(),
                                'fun',
                                [{type, erl_anno:anno(), product, [type()]} |
                                 type()]}.
-type extended_type() :: type() |
                          {type, erl_anno:anno(), bounded_fun,
                                 [function_type() | [constraint()]]} |
                          [extended_type()].
-spec pp_type(extended_type()) -> string().
pp_type(Types = [_|_]) ->
    %% TODO: This is a workaround for the fact that a list is sometimes used in
    %% place of a type. It typically represents a function type with multiple
    %% clauses. We should perhaps represented them as a tuples on the form
    %% {type, Anno, intersection, Types} instead.
    lists:join("; ", lists:map(fun pp_type/1, Types));
pp_type({type, _, bounded_fun, [FunType, []]}) ->
    %% Bounded fun with empty constraints gets printed with a trailing "when"
    %% when pretty-printed as a spec (next clause)
    pp_type(?assert_type(FunType, function_type()));
pp_type(Type = {type, _, bounded_fun, _}) ->
    %% erl_pp can't handle bounded_fun in type definitions
    %% We invent our own syntax here, e.g. "fun((A) -> ok when A :: atom())"
    Form = {attribute, erl_anno:new(0), spec, {{foo, 0}, [Type]}},
    TypeDef = erl_pp:form(Form),
    {match, [S]} = re:run(TypeDef, <<"-spec foo\\s*(.*)\\.\\n*$">>,
                          [{capture, all_but_first, list}, dotall]),
    "fun(" ++ S ++ ")";
pp_type({var, _, TyVar}) ->
    %% See gradualizer_type:af_type_variable/0 and typechecker:new_type_var/0
    if
        is_atom(TyVar) -> atom_to_list(TyVar);
        is_list(TyVar) -> TyVar
    end;
pp_type(Type) ->
    %% erl_pp can handle type definitions, so wrap Type in a type definition
    %% and then take the type from that.
    Form = {attribute, erl_anno:new(0), type, {t, Type, []}},
    TypeDef = erl_pp:form(Form),
    {match, [S]} = re:run(TypeDef, <<"::\\s*(.*)\\.\\n*">>,
                          [{capture, all_but_first, list}, dotall]),
    case S of "INVALID" ++ _ -> error({badarg, Type});
              _ -> ok end,
    S.
    %case erl_anno:file(element(2, Type)) of
    %        undefined -> S;
    %        File      -> S ++ " in " ++ File
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
%% Annotated types as in Name :: Type are also removed.
-type any_t_no_args() :: {type, erl_anno:anno(), any}.
-type unary_op() :: gradualizer_type:af_unary_op(_).
-type binary_op() :: gradualizer_type:af_binary_op(_).

-spec remove_pos(list()) -> list();
                (any_t_no_args()) -> any_t_no_args();
                (af_constraint()) -> af_constraint();
                (type()) -> type();
                (unary_op()) -> unary_op();
                (binary_op()) -> binary_op().
remove_pos([]) ->
    [];
remove_pos([_|_] = L) ->
    lists:map(fun remove_pos/1, L);
remove_pos({type, _, any}) ->
    %% special case for `fun((...) -> R)`,
    %% the only place where `{type, _, any}` can occur
    {type, erl_anno:new(0), any};
remove_pos({type, _, constraint, [{atom, _, is_subtype}, Args]}) ->
    Args = ?assert_type(Args, [type()]),
    L = erl_anno:new(0),
    {type, L, constraint, [{atom, L, is_subtype}, lists:map(fun remove_pos/1, Args)]};
remove_pos({Type, _, Value})
  when Type == atom; Type == integer; Type == char; Type == var ->
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
    {type, erl_anno:new(0), Type, lists:map(fun remove_pos/1, Params)};
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

%% Helper for remove_pos/1. Removes all annotations except filename.
-spec anno_keep_only_filename(erl_anno:anno()) -> erl_anno:anno().
anno_keep_only_filename(Anno) ->
    NewAnno = erl_anno:new(0),
    case erl_anno:file(Anno) of
        undefined -> NewAnno;
        Filename  -> erl_anno:set_file(Filename, NewAnno)
    end.

%% Annotate a user-defined type or record type with a file name.
-spec annotate_user_type(module() | file:filename(), type()) -> type().
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
-spec annotate_user_types(ModOrFile, TypeOrTypes) -> type() | [type()] when
      ModOrFile :: module() | file:filename(),
      TypeOrTypes :: type() | [type()].
annotate_user_types(ModOrFile, TypeOrTypes) ->
    case TypeOrTypes of
        Types when is_list(Types) ->
            [ annotate_user_type(ModOrFile, Type) || Type <- ?assert_type(Types, [type()]) ];
        Type ->
            annotate_user_type(ModOrFile, ?assert_type(Type, type()))
    end.

-spec annotate_user_type_(file:filename(), type()) -> type().
annotate_user_type_(Filename, {user_type, Anno, Name, Params}) ->
    %% Annotate local user-defined type.
    {user_type, erl_anno:set_file(Filename, Anno), Name,
     [annotate_user_type_(Filename, Param) || Param <- Params]};
annotate_user_type_(Filename, {type, Anno, record, RecName = [_]}) ->
    %% Annotate local record type
    {type, erl_anno:set_file(Filename, Anno), record, RecName};
annotate_user_type_(Filename, {type, Anno, T, Params}) when is_list(Params) ->
    {type, Anno, T, [ annotate_user_types(Filename, Param)
                      || Param <- ?assert_type(Params, [type()]) ]};
annotate_user_type_(Filename, {ann_type, Anno, [Var, Type]}) ->
    %% We match Var :: af_anno() and Type :: type() above.
    Type = ?assert_type(Type, type()),
    {ann_type, Anno, [Var, annotate_user_type_(Filename, Type)]};
annotate_user_type_(_Filename, Type) ->
    Type.

-spec get_module_from_annotation(erl_anno:anno()) -> {ok, module()} | none.
get_module_from_annotation(Anno) ->
    case erl_anno:file(Anno) of
        File when is_list(File) ->
            Basename = filename:basename(File, ".erl"),
            {ok, list_to_existing_atom(?assert_type(Basename, string()))};
        undefined ->
            none
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
    %% We have to assert the type below as we're running into the problem documented
    %% with test/known_problems/should_pass/lc_cannot_glb_different_variants.erl.
    %% In other words, the 4th element of a `type()' tuple doesn't have to be a list
    %% and Gradualizer cannot yet use the `is_list(Params)' guard to refine the type.
    {Tag, L, T, [substitute_type_vars(P, TVars) || P <- ?assert_type(Params, list())]};
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
%% > F = fun
%% >         ({atom, _, _} = At, Acc) -> {At, [At | Acc]};
%% >         (Ty, Acc) -> {Ty, Acc}
%% >     end,
%% > {_, [{atom, _, my_atom}]} = typelib:reduce_type(F, [], typelib:parse_type("A :: {my_atom}")).
%%
%% `Fun' can skip traversing parts of the type tree by matching on it
%% and returning `none()' instead of the actual subtree.
%%
%% Example 2 - gather singleton atoms, but skip a particular branch of a union type:
%%
%% > ComplexTy = typelib:parse_type("atom1 | atom2 | "
%% >                                "{complex, integer(), [{atom() | string(), number()}]}"),
%% > F = fun
%% >          ({type, _, tuple, [{atom, _, complex} | _]}, Acc) -> { {type, 0, none, []}, Acc };
%% >          ({atom, _, Name} = Ty, Acc) -> {Ty, [Name | Acc]};
%% >          (Ty, Acc) -> {Ty, Acc}
%% >      end,
%% > {_, [atom2, atom1]} = reduce(F, [], ComplexTy).
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
