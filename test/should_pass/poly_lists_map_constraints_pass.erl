-module(poly_lists_map_constraints_pass).

-gradualizer([solve_constraints]).

%% This is a minimised problem from type checking `gradualizer_db:collect_specs/1'
%% with constraint solving enabled.
-export([map_many/1]).

-export([j/1]).

%% We cannot preserve the nonempty-property across calls to lists:map/2 anymore :(
%% This interferes with the clause choice when calling an intersection-typed function.
-spec map_many({_, nonempty_list()}) -> list().
map_many({_Key, Types}) ->
    lists:map(fun map_elem/1,
              map_specific_list(Types)).

-spec map_elem([A]) -> [A];
              (a | b) -> a.
map_elem(L) when is_list(L) -> L;
map_elem(a) -> a;
map_elem(b) -> a.

-type t_list() :: [a | b].

-spec map_specific_list(t_list()) -> t_list().
map_specific_list(List) ->
    lists:map(fun (E) -> E end, List).

-spec j([binary() | integer()]) -> list().
j(L) ->
    lists:map(fun has_intersection_spec/1, L).

-spec has_intersection_spec(binary()) -> binary();
                           (integer()) -> integer().
has_intersection_spec(B) when is_binary(B) -> B;
has_intersection_spec(I) when is_integer(I) -> I.
