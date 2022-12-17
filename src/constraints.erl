%% @private
-module(constraints).

-export([empty/0, upper/2, lower/2, combine/1, combine/2, add_var/2]).

-export_type([t/0]).

-type type() :: gradualizer_type:abstract_type().

-record(constraints, {
          lower_bounds = #{} :: #{ var() => [type()] },
          upper_bounds = #{} :: #{ var() => [type()] },
          exist_vars   = #{} :: #{ var() => true }
         }).

-type t() :: #constraints{}.
-type var() :: gradualizer_type:gr_type_var().

-spec empty() -> t().
empty() ->
    #constraints{}.

-spec add_var(var(), t()) -> t().
add_var(Var, Cs) ->
    Cs#constraints{ exist_vars = maps:put(Var, true, Cs#constraints.exist_vars) }.

-spec upper(var(), type()) -> t().
upper(Var, Ty) ->
    #constraints{ upper_bounds = #{ Var => [Ty] } }.

-spec lower(var(), type()) -> t().
lower(Var, Ty) ->
    #constraints{ lower_bounds = #{ Var => [Ty] } }.

-spec combine(t(), t()) -> t().
combine(C1, C2) ->
    combine([C1, C2]).

-spec combine([t()]) -> t().
combine([]) ->
    empty();
combine([Cs]) ->
    Cs;
combine([C1, C2 | Cs]) ->
    C = #constraints{ lower_bounds =
                          gradualizer_lib:merge_with(fun (_Var, Tys1, Tys2) ->
                                                 Tys1 ++ Tys2
                                         end
                                        ,C1#constraints.lower_bounds
                                        ,C2#constraints.lower_bounds)
                    , upper_bounds =
                          gradualizer_lib:merge_with(fun (_Var, Tys1, Tys2) ->
                                                 Tys1 ++ Tys2
                                         end
                                        ,C1#constraints.upper_bounds
                                        ,C2#constraints.upper_bounds)
                    , exist_vars =
                          maps:merge(C1#constraints.exist_vars
                                    ,C2#constraints.exist_vars)
                    },
    combine([C | Cs]).

