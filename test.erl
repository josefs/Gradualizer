-module(test).

-compile([export_all]).

-spec collect_specs([any()]) -> [any()].
collect_specs([{attribute,_,spec,Spec}|Forms]) ->
    [Spec|collect_specs(Forms)];
collect_specs([_|Forms]) ->
    collect_specs(Forms);
collect_specs([]) -> [].

get_specs(File) ->
    {ok, Erl} = epp:parse_file(File,[]),
    collect_specs(Erl).

-type type() :: integer | float | boolean | {singleton, atom()}
	      | {range, integer(), integer() }
	      | {tuple, list(type())}
	      | {list, type()}
	      | {function, list(type()), type()}
	      | {union, list(type())}
	      | {intersection, list(type())}
	      | untyped
	      .

-type schema() :: { list(variable()) % parameters
		  , list(variable()) % forall quantified
		  , type()
		  , list(clause()) }.

-type variable() :: integer(). % This will work for now.

-type clause() :: { type () , type() }.
  % We will interpret clauses as subtyping constraints.
  % One option would be to interpret clauses on the form X :: type as
  % type equality, but that wouldn't be very consistent. We can alway
  % recover equality by adding type :: X as a constraint.
  % Perhaps we can argue for a new form of syntax.

-spec type_check_expr(#{},any()) -> any().
type_check_expr(Env, {var, _, Var}) ->
    maps:get(Var, Env);
type_check_expr(Env, {tuple, _, [TS]}) ->
    [ type_check_expr(Env, Expr) || Expr <- TS ];
type_check_expr(Env, {call, _, Name, Args}) ->
    todo.
