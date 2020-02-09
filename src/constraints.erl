-module(constraints).

-export([empty/0, upper/2, lower/2, combine/1, combine/2, add_var/2, solve/2]).

-export_type([constraints/0]).

-include_lib("stdlib/include/assert.hrl").

-type type() :: gradualizer_type:abstract_type().

-record(constraints, {
	  lower_bounds = #{}        :: #{ var() => [type()] },
	  upper_bounds = #{}        :: #{ var() => [type()] },
	  exist_vars   = sets:new() :: sets:set(var())
	 }).

-type constraints() :: #constraints{}.
-type var() :: atom() | string().

-spec empty() -> constraints().
empty() ->
    #constraints{}.

-spec add_var(var(), constraints()) -> constraints().
add_var(Var, Cs) ->
    Cs#constraints{ exist_vars = sets:add_element(Var, Cs#constraints.exist_vars) }.

-spec upper(var(), type()) -> constraints().
upper(Var, Ty) ->
    #constraints{ upper_bounds = #{ Var => [Ty] } }.

-spec lower(var(), type()) -> constraints().
lower(Var, Ty) ->
    #constraints{ lower_bounds = #{ Var => [Ty] } }.

-spec combine(constraints(), constraints()) -> constraints().
combine(C1, C2) ->
    combine([C1, C2]).

-spec combine([constraints()]) -> constraints().
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

-spec solve(constraints(), typechecker:tenv()) -> {constraints()
						  , {#{var() => type()}
						    ,#{var() => type()}}}.
solve(Constraints, TEnv) ->
    ElimVars = Constraints#constraints.exist_vars,
    WorkList =
	[ {LB, UB} || E <- sets:to_list(ElimVars),
		      LB <- maps:get(E,Constraints#constraints.lower_bounds, []),
		      UB <- maps:get(E,Constraints#constraints.upper_bounds, []) ],
    Cs = solve_loop(WorkList, sets:new(), Constraints, ElimVars, TEnv),
    GlbSubs = fun(_Var, Tys) ->
		      {Ty, _C} = typechecker:glb(Tys, TEnv),
		      % TODO: Don't throw away the constraints
		      Ty
	      end,
    LubSubst = fun(_Var, Tys) ->
		       Ty = typechecker:lub(Tys, TEnv),
		       Ty
	       end,
    % TODO: What if the substition contains occurrences of the variables we're eliminating
    % in the range of the substitution?
    Subst = {maps:map(GlbSubs,
		      maps:with(sets:to_list(ElimVars), Cs#constraints.upper_bounds))
	    ,maps:map(LubSubst,
		      maps:with(sets:to_list(ElimVars), Cs#constraints.lower_bounds))
	    },
    C = #constraints{
	   upper_bounds = maps:without(sets:to_list(ElimVars), Cs#constraints.upper_bounds),
	   lower_bounds = maps:without(sets:to_list(ElimVars), Cs#constraints.lower_bounds),
	   exist_vars = sets:new()
	  },
    {C, Subst}.

solve_loop([], _, Constraints, _, _) ->
    Constraints;
solve_loop([I={LB,UB}|WL], Seen, Constraints, ElimVars, TEnv) ->
    case sets:is_element(I, Seen) of
	true ->
	    solve_loop(WL, Seen, Constraints, ElimVars, TEnv);
	false ->

	    C = case typechecker:subtype(LB, UB, TEnv) of
		    false ->
			throw({constraint_error, LB, UB});
		    {true, Cs} ->
			Cs
		end,

	    % Subtyping should not create new existential variables
	    ?assert(sets:is_empty(C#constraints.exist_vars)),

	    ELowerBounds = maps:with(sets:to_list(ElimVars), C#constraints.lower_bounds),
	    EUpperBounds = maps:with(sets:to_list(ElimVars), C#constraints.upper_bounds),

	    Constraints2 =
		#constraints{
		   lower_bounds =
		       gradualizer_lib:merge_with(fun app/3, Constraints#constraints.lower_bounds
							   , C#constraints.lower_bounds),
		   upper_bounds =
		       gradualizer_lib:merge_with(fun app/3, Constraints#constraints.upper_bounds
							   , C#constraints.upper_bounds)
		  },
	    NewWL =
		[ {Lower, Upper} || {EVar, Lowers} <- maps:to_list(ELowerBounds),
				    Lower <- Lowers,
				    Upper <- maps:get(EVar, Constraints2#constraints.upper_bounds) ] ++
		[ {Lower, Upper} || {Evar, Uppers} <- maps:to_list(EUpperBounds),
				    Upper <- Uppers,
				    Lower <- maps:get(Evar, Constraints2#constraints.lower_bounds) ] ++
		WL,
	    solve_loop(NewWL, sets:add_element(I,Seen), Constraints2, ElimVars, TEnv)
    end.

app(_, Xs, Ys) ->
    Xs ++ Ys.
