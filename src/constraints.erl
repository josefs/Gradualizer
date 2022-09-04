%% @private
-module(constraints).

-export([empty/0, vars/1, upper/2, lower/2, combine/1, combine/2, add_var/2, solve/3]).

-export_type([t/0]).

-include_lib("stdlib/include/assert.hrl").

-type type() :: gradualizer_type:abstract_type().

-record(constraints, { lower_bounds = #{} :: #{ var() => [type()] },
                       upper_bounds = #{} :: #{ var() => [type()] },
                       exist_vars   = #{} :: #{ var() => true } }).

-type t() :: #constraints{}.
-type var() :: gradualizer_type:gr_type_var().

-spec empty() -> t().
empty() ->
    #constraints{}.

-spec vars(#{ var() => true }) -> #constraints{}.
vars(Vars) ->
    #constraints{ exist_vars = Vars }.

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
    LBounds = gradualizer_lib:merge_with(fun (_Var, Tys1, Tys2) ->
                                                 Tys1 ++ Tys2
                                         end,
                                         C1#constraints.lower_bounds,
                                         C2#constraints.lower_bounds),
    UBounds = gradualizer_lib:merge_with(fun (_Var, Tys1, Tys2) ->
                                                 Tys1 ++ Tys2
                                         end,
                                         C1#constraints.upper_bounds,
                                         C2#constraints.upper_bounds),
    EVars = maps:merge(C1#constraints.exist_vars, C2#constraints.exist_vars),
    C = #constraints{lower_bounds = LBounds,
                     upper_bounds = UBounds,
                     exist_vars = EVars},
    combine([C | Cs]).

-spec solve(t(), erl_anno:anno(), typechecker:env()) -> R when
      R :: {t(), {#{var() => type()}, #{var() => type()}}}.
solve(Constraints, Anno, Env) ->
    ElimVars = Constraints#constraints.exist_vars,
    WorkList = [ {LB, UB} || E <- maps:keys(ElimVars),
                             LB <- maps:get(E, Constraints#constraints.lower_bounds, []),
                             UB <- maps:get(E, Constraints#constraints.upper_bounds, []) ],
    Cs = solve_loop(WorkList, maps:new(), Constraints, ElimVars, Anno, Env),
    GlbSubs = fun(_Var, Tys) ->
                      {Ty, _C} = typechecker:glb(Tys, Env),
                      % TODO: Don't throw away the constraints
                      Ty
              end,
    LubSubs = fun(_Var, Tys) ->
                      Ty = typechecker:lub(Tys, Env),
                      Ty
              end,
    % TODO: What if the substition contains occurrences of the variables we're eliminating
    % in the range of the substitution?
    Subst = { maps:map(GlbSubs, maps:with(maps:keys(ElimVars), Cs#constraints.upper_bounds)),
              maps:map(LubSubs, maps:with(maps:keys(ElimVars), Cs#constraints.lower_bounds)) },
    UBounds = maps:without(maps:keys(ElimVars), Cs#constraints.upper_bounds),
    LBounds = maps:without(maps:keys(ElimVars), Cs#constraints.lower_bounds),
    C = #constraints{upper_bounds = UBounds,
                     lower_bounds = LBounds,
                     exist_vars = maps:new()},
    {C, Subst}.

solve_loop([], _, Constraints, _, _, _) ->
    Constraints;
solve_loop([I = {LB, UB} | WL], Seen, Constraints, ElimVars, Anno, Env) ->
    case maps:is_key(I, Seen) of
        true ->
            solve_loop(WL, Seen, Constraints, ElimVars, Anno, Env);
        false ->
            C = case typechecker:subtype(LB, UB, Env) of
                    false ->
                        throw({constraint_error, Anno, LB, UB});
                    {true, Cs} ->
                        Cs
                end,

            % Subtyping should not create new existential variables
            ?assert(C#constraints.exist_vars == #{}),

            ELowerBounds = maps:with(maps:keys(ElimVars), C#constraints.lower_bounds),
            EUpperBounds = maps:with(maps:keys(ElimVars), C#constraints.upper_bounds),

            LBounds = gradualizer_lib:merge_with(fun app/3, Constraints#constraints.lower_bounds,
                                                 C#constraints.lower_bounds),
            UBounds = gradualizer_lib:merge_with(fun app/3, Constraints#constraints.upper_bounds,
                                                 C#constraints.upper_bounds),
            Constraints2 = #constraints{lower_bounds = LBounds,
                                        upper_bounds = UBounds},
            NewWL = ([ {Lower, Upper} || {EVar, Lowers} <- maps:to_list(ELowerBounds),
                                         Lower <- Lowers,
                                         Upper <- maps:get(EVar, Constraints2#constraints.upper_bounds, []) ] ++
                     [ {Lower, Upper} || {Evar, Uppers} <- maps:to_list(EUpperBounds),
                                         Upper <- Uppers,
                                         Lower <- maps:get(Evar, Constraints2#constraints.lower_bounds, []) ] ++
                     WL),
            solve_loop(NewWL, maps:put(I, true, Seen), Constraints2, ElimVars, Anno, Env)
    end.

app(_, Xs, Ys) ->
    Xs ++ Ys.
