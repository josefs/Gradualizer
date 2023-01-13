-module(poly_union_lower_bound_fail).

-gradualizer([solve_constraints]).

-export([i/1,
         j/1,
         k/1,
         l/1]).

-spec i([binary() | integer()]) -> [integer()].
i(L) ->
    lists:map(fun erlang:integer_to_list/1, L).

%% We expect an error like:
%%
%%   Lower bound binary() | integer() | string() of type variable T_123
%%   on line 456 is not a subtype of binary() | integer()
-spec j([binary() | integer() | string()]) -> [integer()].
j(L) ->
    lists:map(fun takes_a_union/1, L).
-spec takes_a_union(binary() | integer()) -> integer().
takes_a_union(B) when is_binary(B) -> binary_to_integer(B);
takes_a_union(I) when is_integer(I) -> I.

%% We expect an error like:
%%
%%   Lower bound binary() | integer() | string() of type variable T_123
%%   on line 456 is not a subtype of binary() | integer()
-spec k([binary() | integer() | string()]) -> list().
k(L) ->
    lists:map(fun has_intersection_spec/1, L).

-spec has_intersection_spec(binary()) -> binary();
                           (integer()) -> integer().
has_intersection_spec(B) when is_binary(B) -> B;
has_intersection_spec(I) when is_integer(I) -> I.

-spec l([binary() | integer()]) -> [integer()].
l(L) ->
    lists:map(fun has_intersection_spec/1, L).
