-module(lists_map_constraints_pass).

%% This is a minimised problem from type checking `gradualizer_db:collect_specs/1'
%% with constraint solving enabled.

-export([map_many/1]).

-spec map_many({_, nonempty_list()}) -> nonempty_list().
map_many({_Key, Types}) ->
    lists:map(fun map_elem/1,
              map_specific_list(Types)).

-spec map_elem([A]) -> [A];
              (a | b) -> a.
map_elem(L) when is_list(L) -> L;
map_elem(a) -> a;
map_elem(b) -> a.

-type t_list() :: [a | b, ...].

-spec map_specific_list(t_list()) -> t_list().
map_specific_list(List) ->
    lists:map(fun (E) -> E end, List).
