-module(typechecker).

-compile([export_all]).

-spec type_check_expr(#{},#{},any()) -> any().
type_check_expr(_FEnv, VEnv, {var, _, Var}) ->
    maps:get(Var, VEnv);
type_check_expr(FEnv, VEnv, {tuple, _, [TS]}) ->
    [ type_check_expr(FEnv, VEnv, Expr) || Expr <- TS ];
type_check_expr(FEnv, VEnv, {call, _, Name, _Args}) ->
    type_check_fun(FEnv, VEnv, Name).

type_check_fun(FEnv, _VEnv, {atom, _, Name}) ->
    maps:get(FEnv, Name);
type_check_fun(FEnv, _VEnv, {remote, _, {atom,_,Module}, {atom,_,Fun}}) ->
    maps:get(FEnv,{Module,Fun});
type_check_fun(FEnv, VEnv, Expr) ->
    type_check_expr(FEnv, VEnv, Expr).


type_check_clauses(_FEnv, _VEnv, _Clauses) ->
    foo.

infer_clauses(FEnv, VEnv, Clauses) ->
    merge_types(lists:map(fun (Clause) ->
				  infer_clause(FEnv, VEnv, Clause)
			  end, Clauses)).

infer_clause(_FEnv, _VEnv, {clause, _, _Args, _, _Expr}) ->
    apa.

type_check_function(FEnv, {function,_, Name, _NArgs, Clauses}) ->
    case maps:find(Name, FEnv) of
	{ok, _Type} -> apa;
	error ->
	    Types = type_check_clauses(FEnv, #{}, Clauses),
	    merge_types(Types)
    end.

type_check_file(File) ->
    {ok, Forms} = epp:parse_file(File,[]),
    {Specs, Funs} = collect_specs_and_functions(Forms),
    FEnv = create_fenv(Specs),
    lists:map(fun (Function) ->
		      type_check_function(FEnv, Function) end, Funs).

collect_specs_and_functions(Forms) ->
    aux(Forms,[],[]).
aux([], Specs, Funs) ->
    {Specs, Funs};
aux([Fun={function, _, _, _, _} | Forms], Specs, Funs) ->
    aux(Forms, Specs, [Fun | Funs]);
aux([{attribute, _, spec, Spec} | Forms], Specs, Funs) ->
    aux(Forms, [Spec | Specs], Funs);
aux([_|Forms], Specs, Funs) ->
    aux(Forms, Specs, Funs).

merge_types(apa) ->
    error.

create_fenv([{{Name,_},[Type]}|Specs]) ->
    (create_fenv(Specs))#{ Name => Type };
create_fenv([{{Name,_},_}|_]) ->
    throw({multiple_types_not_supported,Name});
create_fenv([]) ->
    #{}.
