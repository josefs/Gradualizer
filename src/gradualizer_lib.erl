% This module contains useful helper functions.

-module(gradualizer_lib).

-export([merge_with/3, top_sort/1, get_type_definition/3,
         pick_value/2, fold_ast/3, get_ast_children/1,
         empty_tenv/0, create_tenv/3]).
-export_type([graph/1, tenv/0]).

%% Type environment, passed around while comparing compatible subtypes.
-type tenv() :: #{ module => module(),
                   types := #{{Ty :: atom(), arity()} => {Params :: [atom()],
                                                          Body :: gradualizer_type:abstract_type()}},
                   records := #{Rec :: atom() => [typechecker:typed_record_field()]} }.

%% Pattern macros
-define(type(T), {type, _, T, []}).
-define(type(T, A), {type, _, T, A}).

%% Number of space used when prettyprinting records for nesting
-define(PP_RECORD_NESTING_OFFSET, 2).

%% merge_with for maps. Similar to merge_with for dicts.
%% Arguably, this function should be in OTP.
merge_with(F, M1, M2) ->
    case maps:size(M1) < maps:size(M2) of
        true ->
            maps:fold(fun (K, V1, M) ->
                              maps:update_with(K, fun (V2) -> F(K, V1, V2) end, V1, M)
                      end, M2, M1);
        false ->
            maps:fold(fun (K, V2, M) ->
                              maps:update_with(K, fun (V1) -> F(K, V1, V2) end, V2, M)
                      end, M1, M2)
    end.

%% -- Topological sort

-type graph(Node) :: #{Node => [Node]}. %% List of incoming edges (dependencies).

%% Topologically sorted strongly-connected components of a graph.
-spec top_sort(graph(Node)) -> [{cyclic, [Node]} | {acyclic, Node}].
top_sort(Graph) ->
    Trees = dfs(Graph, lists:reverse(postorder(dff(reverse_graph(Graph))))),
    Decode = fun(T) ->
        case postorder(T) of
            [I] -> case lists:member(I, maps:get(I, Graph, [])) of
                       true  -> {cyclic, [I]};
                       false -> {acyclic, I}
                   end;
            Is -> {cyclic, Is}
        end end,
    lists:map(Decode, Trees).

%% Depth first spanning forest of a graph.
dff(Graph) ->
    dfs(Graph, maps:keys(Graph)).

dfs(Graph, Vs) ->
    {_, Trees} = dfs(Graph, #{}, Vs, []),
    Trees.

dfs(_Graph, Visited, [], Trees) -> {Visited, lists:reverse(Trees)};
dfs(Graph, Visited, [V | Vs], Trees) ->
    case maps:is_key(V, Visited) of
        true  -> dfs(Graph, Visited, Vs, Trees);
        false ->
            {Visited1, Tree} = dfs1(Graph, Visited#{ V => true }, V),
            dfs(Graph, Visited1, Vs, [Tree | Trees])
    end.

dfs1(Graph, Visited, V) ->
    Ws = maps:get(V, Graph, []),
    {Visited1, Trees} = dfs(Graph, Visited, Ws, []),
    {Visited1, {V, Trees}}.

%% Post-order traversal of a tree/forest.
postorder(Tree = {_, _}) -> postorder([Tree]);
postorder(Trees) when is_list(Trees) -> postorder(Trees, []).

postorder([], Acc) -> Acc;
postorder([{V, Trees1} | Trees], Acc) ->
    postorder(Trees1, [V | postorder(Trees, Acc)]).

from_edges(Is, Es) ->
    lists:foldl(fun({I, J}, G) ->
            maps:update_with(I, fun(Js) -> lists:umerge([J], Js) end, [J], G)
        end, maps:from_list([ {I, []} || I <- Is ]), Es).

reverse_graph(G) ->
    from_edges(maps:keys(G), [ {J, I} || {I, Js} <- maps:to_list(G), J <- Js ]).


%% Look up `UserTy' definition:
%% first in `gradualizer_db', then, if not found, in provided `Types' map.
%% `UserTy' is actually an unexported `gradualizer_type:af_user_defined_type()'.

-spec get_type_definition(UserTy, TEnv, Opts) -> {ok, Ty} | opaque | not_found when
      UserTy :: gradualizer_type:abstract_type(),
      TEnv :: tenv(),
      Opts :: [annotate_user_types],
      Ty :: gradualizer_type:abstract_type().
get_type_definition({remote_type, _Anno, [{atom, _, Module}, {atom, _, Name}, Args]}, _TEnv, _Opts) ->
    gradualizer_db:get_type(Module, Name, Args);
get_type_definition({user_type, Anno, Name, Args}, TEnv, Opts) ->
    %% Let's check if the type is a known remote type.
    case typelib:get_module_from_annotation(Anno) of
        {ok, Module} ->
            gradualizer_db:get_type(Module, Name, Args);
        none ->
            %% Let's check if the type is defined in the context of this module.
            case maps:get({Name, length(Args)}, maps:get(types, TEnv), not_found) of
                {Params, Type0} ->
                    VarMap = maps:from_list(lists:zip(Params, Args)),
                    Type2 = case proplists:is_defined(annotate_user_types, Opts) of
                                true ->
                                    Type1 = typelib:annotate_user_types(maps:get(module, TEnv), Type0),
                                    typelib:substitute_type_vars(Type1, VarMap);
                                false ->
                                    typelib:substitute_type_vars(Type0, VarMap)
                            end,
                    {ok, Type2};
                not_found ->
                    not_found
            end
    end.


%% Given a type `Ty', pick a value of that type.
%% Used in exhaustiveness checking to show an example value
%% which is not covered by the cases.

-spec pick_value(Ty, TEnv) -> AbstractVal when
      Ty :: gradualizer_type:abstract_type(),
      TEnv :: tenv(),
      AbstractVal :: gradualizer_type:abstract_expr().
pick_value(List, TEnv) when is_list(List) ->
    [pick_value(Ty, TEnv) || Ty <- List ];
pick_value(?type(integer), _TEnv) ->
    {integer, erl_anno:new(0), 0};
pick_value(?type(char), _TEnv) ->
    {char, erl_anno:new(0), $a};
pick_value(?type(non_neg_integer), _TEnv) ->
    {integer, erl_anno:new(0), 0};
pick_value(?type(pos_integer), _TEnv) ->
    {integer, erl_anno:new(0), 0};
pick_value(?type(neg_integer), _TEnv) ->
    {integer, erl_anno:new(0), -1};
pick_value(?type(float), _TEnv) ->
    {float, erl_anno:new(0), -1};
pick_value(?type(atom), _TEnv) ->
    {atom, erl_anno:new(0), a};
pick_value({atom, _, A}, _TEnv) ->
    {atom, erl_anno:new(0), A};
pick_value({ann_type, _, [_, Ty]}, TEnv) ->
    pick_value(Ty, TEnv);
pick_value(?type(union, [Ty|_]), TEnv) ->
    pick_value(Ty, TEnv);
pick_value(?type(tuple, any), _TEnv) ->
    {tuple, erl_anno:new(0), []};
pick_value(?type(tuple, Tys), TEnv) ->
    {tuple, erl_anno:new(0), [pick_value(Ty, TEnv) || Ty <- Tys]};
pick_value(?type(record, [{atom, _, RecordName}]), _TEnv) ->
    {record, erl_anno:new(0), RecordName, []};
pick_value(?type(record, [{atom, _, RecordName} | Tys]), TEnv) ->
    MFields = [
        {record_field, erl_anno:new(0), {atom, erl_anno:new(0), FieldName}, pick_value(Ty, TEnv)}
        || ?type(field_type, [{atom, _, FieldName}, Ty]) <- Tys
    ],
    {record, erl_anno:new(0), RecordName, MFields};
pick_value(?type(map, Assocs), TEnv) ->
    NewAssocs = [ {AssocTag, Anno, pick_value(KTy, TEnv), pick_value(VTy, TEnv)}
                  || {type, Anno, AssocTag, [KTy, VTy]} <- Assocs ],
    {map, erl_anno:new(0), NewAssocs};
pick_value(?type(boolean), _TEnv) ->
    {atom, erl_anno:new(0), false};
pick_value(?type(list), _TEnv) ->
    {nil, erl_anno:new(0)};
pick_value(?type(list,_), _TEnv) ->
    {nil, erl_anno:new(0)};
pick_value(?type(nil), _TEnv) ->
    {nil, erl_anno:new(0)};
%% The ?type(range) is a different case because the type range
%% ..information is not encoded as an abstract_type()
%% i.e. {type, Anno, range, [{integer, Anno2, Low}, {integer, Anno3, High}]}
pick_value(?type(range, [{_TagLo, _, neg_inf}, Hi = {_TagHi, _, _Hi}]), _TEnv) ->
    %% pick_value(Hi, TEnv);
    Hi;
pick_value(?type(range, [Lo = {_TagLo, _, _Lo}, {_TagHi, _, _Hi}]), _TEnv) ->
    %% pick_value(Lo, TEnv).
    Lo;
pick_value(Type, TEnv)
  when element(1, Type) =:= remote_type; element(1, Type) =:= user_type ->
    {Kind, Anno, Name, Args} = case Type of
                                   {remote_type, Anno, [_, {atom, _, Name}, Args]} ->
                                       {remote_type, Anno, Name, Args};
                                   {user_type, Anno, Name, Args} ->
                                       {user_type, Anno, Name, Args}
                               end,
    case get_type_definition(Type, TEnv, [annotate_user_types]) of
        {ok, Ty} ->
            pick_value(Ty, TEnv);
        opaque ->
            UniqueOpaque = list_to_atom("Opaque" ++ integer_to_list(erlang:unique_integer([positive]))),
            {var, Anno, UniqueOpaque};
        not_found ->
            throw({undef, Kind, Anno, {Name, length(Args)}})
    end.


%% ------------------------------------------------
%% Functions for working with abstract syntax trees
%% ------------------------------------------------

%% erl_parse:erl_parse_tree() is documented but not exported :-(
-type erl_parse_tree() :: erl_parse:abstract_clause()
                        | erl_parse:abstract_expr()
                        | erl_parse:abstract_form()
                        | erl_parse:abstract_type().

%% Folds a function over an Erlang abstract syntax tree.  The fun is applied to
%% each node (tuple) in the AST, which is traversed in depth first order.
-spec fold_ast(Fun, AccIn, Ast) -> AccOut
        when Fun    :: fun((tuple(), Acc) -> Acc),
             Ast    :: erl_parse_tree() | [erl_parse_tree()],
             AccIn  :: Acc,
             AccOut :: Acc.
fold_ast(Fun, AccIn, [X|Xs]) ->
    Acc = fold_ast(Fun, AccIn, X),
    fold_ast(Fun, Acc, Xs);
fold_ast(Fun, AccIn, Node) when is_tuple(Node) ->
    Acc = Fun(Node, AccIn),
    fold_ast(Fun, Acc, get_ast_children(Node));
fold_ast(_Fun, Acc, _LiteralEtc) ->
    Acc.

%% Returns the children of an AST node
get_ast_children({clauses, Clauses}) ->
    %% This one doesn't have an annotation
    Clauses;
get_ast_children(Node) ->
    [_Tag, _Anno | Children] = tuple_to_list(Node),
    Children.


%% ------------------------------------------------
%% Type environment
%% ------------------------------------------------

-spec empty_tenv() -> tenv().
empty_tenv() ->
    #{types => #{},
      records => #{}}.

-spec create_tenv(_, _, _) -> tenv().
create_tenv(Module, TypeDefs, RecordDefs) when is_atom(Module) ->
    TypeMap =
        maps:from_list([begin
                            Id       = {Name, length(Vars)},
                            Params   = [VarName || {var, _, VarName} <- Vars],
                            {Id, {Params, typelib:remove_pos(Body)}}
                        end || {Name, Body, Vars} <- TypeDefs]),
    RecordMap =
        maps:from_list([{Name, [{typed_record_field, Field, typelib:remove_pos(Type)}
                                || {typed_record_field, Field, Type}
                                       <- lists:map(fun absform:normalize_record_field/1,
                                                    Fields)]}
                         || {Name, Fields} <- RecordDefs]),
    #{module => Module,
      types => TypeMap,
      records => RecordMap}.
