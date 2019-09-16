% This module contains useful helper functions.

-module(gradualizer_lib).

-export([merge_with/3, top_sort/1, pick_value/1]).
-export_type([graph/1]).

%% Pattern macros
-define(type(T), {type, _, T, []}).
-define(type(T, A), {type, _, T, A}).

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
pick_value(?type(list)) ->
    [];
pick_value(?type(list,_)) ->
    [];
pick_value(?type(range, [{_TagLo, _, neg_inf}, {_TagHi, _, Hi}])) ->
    Hi;
pick_value(?type(range, [{_TagLo, _, Lo}, {_TagHi, _, _Hi}])) ->
    Lo.
