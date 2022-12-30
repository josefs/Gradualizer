-module(poly_lists_map_constraints_should_pass).

-gradualizer([solve_constraints]).

%% This is a minimised problem from type checking `gradualizer_db:collect_specs/1'
%% with constraint solving enabled.
-export([map_many/1]).

-export([j/1]).

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

-spec j([binary() | integer()]) -> list().
j(L) ->
    lists:map(fun takes_an_intersection/1, L).

-spec takes_an_intersection(binary()) -> binary();
                           (integer()) -> integer().
takes_an_intersection(B) when is_binary(B) -> B;
takes_an_intersection(I) when is_integer(I) -> I.
