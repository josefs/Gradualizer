% This module contains useful helper functions.

-module(gradualizer_lib).

-export([merge_with/3, top_sort/1, pick_value/1, fold_ast/3, get_ast_children/1]).
-export_type([graph/1]).

%% Pattern macros
-define(type(T), {type, _, T, []}).
-define(type(T, A), {type, _, T, A}).

%% Prettyprinting Records nesting spaces (or tabs)
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


% Given a type, pick a value of that type.
% Used in exhaustiveness checking to show an example value
% which is not covered by the cases.
pick_value(List) when is_list(List) ->
    [pick_value(Ty) || Ty <- List ];
pick_value(?type(integer)) ->
    0;
pick_value(?type(char)) ->
    $a;
pick_value(?type(non_neg_integer)) ->
    0;
pick_value(?type(pos_integer)) ->
    1;
pick_value(?type(neg_integer)) ->
    -1;
pick_value(?type(float)) ->
    0.0;
pick_value(?type(atom)) ->
    a;
pick_value({atom, _, A}) ->
    A;
pick_value({ann_type, _, [_, Ty]}) ->
    pick_value(Ty);
pick_value(?type(union, [Ty|_])) ->
    pick_value(Ty);
pick_value(?type(tuple, any)) ->
    {};
pick_value(?type(tuple, Tys)) ->
    list_to_tuple([pick_value(Ty) || Ty <- Tys]);
pick_value(Rec = ?type(record, _)) ->
    prettyprint(Rec);
pick_value(?type(list)) ->
    [];
pick_value(?type(list,_)) ->
    [];
pick_value(?type(nil)) ->
    [];
pick_value(?type(range, [{_TagLo, _, neg_inf}, {_TagHi, _, Hi}])) ->
    Hi;
pick_value(?type(range, [{_TagLo, _, Lo}, {_TagHi, _, _Hi}])) ->
    Lo.

%% Prettyprints a ?type(record)
%%
%% What to expect:
%% ```
%%   #record_name{
%%     field_one = #nested_record{
%%       nested_field = 1
%%     },
%%     field_two = some_atom
%%   }
%% ```
prettyprint(Record) ->
    Doc =
        case pp_record(Record) of
            X when is_list(X) -> lists:foldr(fun prettypr:above/2, prettypr:empty(), X);
            X -> X
        end,
    prettypr:format(Doc).

pp_record(?type(record, [{atom, _, RecName} | Fields])) ->
    PPFields = pp_record_fields(Fields),
    %% We return a list instead of a document to achieve "C-style" nesting of tuples/records.
    %% The list is embedded directly in a `field_type`'s fields for instance
    %% to align the field nesting with the name of the field;
    %% otherwise, the nesting would be aligned with the name of the record.
    [
        prettypr:text(["#", atom_to_list(RecName), "{"]),
        prettypr:nest(?PP_RECORD_NESTING_OFFSET, PPFields),
        prettypr:text("}")
    ];
pp_record(?type(field_type, [{atom, _, FieldName}, FieldValue])) ->
    Val = lists:flatten([pp_record(FieldValue)]),
    prettypr:par([
        prettypr:text([atom_to_list(FieldName), " ="])
        | Val
    ], 0);
pp_record(Value) ->
    prettypr:text(io_lib:format("~p", [pick_value(Value)])).

pp_record_fields(Fields) ->
    R = pp_record_fields(Fields, [], []),
    lists:foldl(fun prettypr:above/2, prettypr:empty(), R).
pp_record_fields([], [X, sep | Tail], Acc) ->
    pp_record_fields([], Tail, [prettypr:beside(X, prettypr:text(",")) | Acc]);
pp_record_fields([], [X | Tail], Acc) ->
    pp_record_fields([], Tail, [X | Acc]);
pp_record_fields([], [], Acc) ->
    Acc;
pp_record_fields([X | Tail], [], []) ->
    pp_record_fields([], [
        pp_record(X)
        |
        lists:foldr(fun (Field, Acc) ->
            %% prettypr:above(prettypr:beside(Acc, prettypr:text(",")), pp_record(Field))
            [sep, pp_record(Field) | Acc]
        end, [], Tail)
    ], []);
    %% lists:foldr(fun lists:flatmap(fun pp_record/1, [X | Tail]);
pp_record_fields(_, _, _) ->
    prettypr:empty().


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
