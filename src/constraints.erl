%% @private
-module(constraints).

%% The `constraint' module tracks lower and upper bounds (i.e., constraints) for type variables.
%% Each bound is a single type. A type variable may have a lower bound, an upper bound, or both.
%% Note that the bounds cannot contain any type variables, they are just plain monomorphic types.
%% For more information on constraint solving and typechecking polymorphic calls, see
%% `typechecker:type_check_poly_call/4'.

-export([empty/0,
         upper/2,
         lower/2,
         combine/2, combine/3,
         satisfiable/2]).

-export_type([t/0, var/0]).

-type type() :: gradualizer_type:abstract_type().

-include("constraints.hrl").

-type t() :: #constraints{}.
-type var() :: atom().
-type env() :: typechecker:env().

-spec empty() -> t().
empty() ->
    #constraints{}.

-spec upper(var(), type()) -> t().
upper(Var, Ty) ->
    #constraints{ upper_bounds = #{ Var => Ty } }.

-spec lower(var(), type()) -> t().
lower(Var, Ty) ->
    #constraints{ lower_bounds = #{ Var => Ty } }.

-spec combine(t(), t(), env()) -> t().
combine(Cs1, Cs2, Env) ->
    combine([Cs1, Cs2], Env).

-spec combine([t()], env()) -> t().
combine([], _Env) ->
    empty();
combine([Cs], _Env) ->
    Cs;
combine([Cs1, Cs2 | Css], Env) ->
    Cs = do_combine(Cs1, Cs2, Env),
    combine([Cs | Css], Env).

-spec do_combine(t(), t(), env()) -> t().
do_combine(Cs1, Cs2, Env) ->
    MergeLBounds = fun (_Var, T1, T2) -> typechecker:lub([T1, T2], Env) end,
    MergeUBounds = fun (_Var, T1, T2) -> typechecker:glb(T1, T2, Env) end,
    LBounds = gradualizer_lib:merge_with(MergeLBounds,
                                         Cs1#constraints.lower_bounds,
                                         Cs2#constraints.lower_bounds),
    UBounds = gradualizer_lib:merge_with(MergeUBounds,
                                         Cs1#constraints.upper_bounds,
                                         Cs2#constraints.upper_bounds),
    Combined = #constraints{lower_bounds = LBounds,
                 upper_bounds = UBounds},
    Combined.


-spec variables(t()) -> [var()].
variables(#constraints{ upper_bounds = UBounds, lower_bounds = LBounds }) ->
    gradualizer_lib:uniq(maps:keys(UBounds) ++ maps:keys(LBounds)).


%% Checks that all lower bounds are subtypes of respective upper bounds.
-spec satisfiable(t(), env()) -> true | {false, var(), type(), type()}.
satisfiable(Cs, Env) ->
    Vars = variables(Cs),
    try
        lists:foreach(fun (Var) ->
            LBound = maps:get(Var, Cs#constraints.lower_bounds, typechecker:type(none)),
            UBound = maps:get(Var, Cs#constraints.upper_bounds, typechecker:type(top)),
            case typechecker:subtype(LBound, UBound, Env) of
                false -> throw({not_subtype, Var, LBound, UBound});
                true -> ok
            end
        end, Vars),
        true
    catch
        {not_subtype, Var, LBound, UBound} -> {false, Var, LBound, UBound}
    end.
