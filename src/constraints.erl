-module(constraints).

-export([empty/0, upper/2, lower/2, combine/1, combine/2, convert/1, add_var/2]).

-export_type([constraints/0]).

-type type() :: erl_parse:abstract_type().

-record(constraints, {
          lower_bounds = #{}        :: #{ atom() => [type()] },
          upper_bounds = #{}        :: #{ atom() => [type()] },
          exist_vars   = sets:new() :: sets:set(string())
         }).

-type constraints() :: #constraints{}.

empty() ->
    #constraints{}.

add_var(Var, Cs) ->
    Cs#constraints{ exist_vars = sets:add_element(Var, Cs#constraints.exist_vars) }.

upper(Var, Ty) ->
    #constraints{ upper_bounds = #{ Var => [Ty] } }.

lower(Var, Ty) ->
    #constraints{ lower_bounds = #{ Var => [Ty] } }.

combine(C1, C2) ->
    combine([C1, C2]).

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
                          sets:union(C1#constraints.exist_vars
                                    ,C2#constraints.exist_vars)
                    },
    combine([C | Cs]).

convert(Cs) when is_list(Cs) ->
    combine(lists:map(fun convert/1, Cs));

convert({type, _,constraint,[{atom, _,is_subtype},[TV = {var, _, V}
                                                  ,TW = {var, _, W}]]}) ->
    #constraints{ upper_bounds = #{ V => [TW] }
                , lower_bounds = #{ W => [TV] } };
convert({type, _,constraint,[{atom, _,is_subtype},[{var, _, V},Ty]]}) ->
    #constraints{ upper_bounds = #{ V => [Ty] } }.
