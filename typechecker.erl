-module(typechecker).

-compile([export_all]).

%% Data collected from epp parse tree
-record(parsedata, {
	  module             :: atom(),
	  export_all = false :: boolean(),
	  exports    = []    :: [{atom(), integer()}],
	  specs      = []    :: list(),
	  types      = []    :: list(),
	  opaques    = []    :: list(),
	  records    = []    :: list(),
	  functions  = []    :: list()
	 }).

% Subtyping compatibility
% The first argument is a "compatible subtype" of the second.

-spec subtype(any(), any()) -> boolean(). % {ok, any()} | false.
subtype(Ty1, Ty2) ->
    case catch compat(remove_pos(Ty1),remove_pos(Ty2), sets:new(), maps:new()) of
	Tuple when element(1,Tuple) == type_error -> false;
	{_, _Constraints} -> %{ok, Constraints}
	    true
    end.

subtypes([], []) ->
    true;
subtypes([Ty1|Tys1], [Ty2|Tys2]) ->
    subtype(Ty1, Ty2) andalso subtypes(Tys1, Tys2).



% This function throws an exception in case of a type error

%% The functions compat and compat_ty are mutually recursive.
%% The main entry point is compat and all recursive calls should go via compat.
%% The function compat_ty is just a convenience function to be able to
%% pattern match on types in a nice way.
compat(Ty1, Ty2, A, TEnv) ->
    case sets:is_element({Ty1, Ty2}, A) of
	true ->
	    ret(A);
	false ->
	    compat_ty(Ty1, Ty2, sets:add_element({Ty1, Ty2}, A), TEnv)
    end.

compat_ty({type, any, []}, _, A, _TEnv) ->
    ret(A);
compat_ty(_, {type, any ,[]}, A, _TEnv) ->
    ret(A);
%% Term is the top of the subtyping relation
compat_ty(_, {type, term, []}, A, _TEnv) ->
    ret(A);
%% None is the bottom of the subtyping relation
compat_ty({type, none, []}, _, A, _TEnv) ->
    ret(A);

% TODO: There are several kinds of fun types.
% Add support for them all eventually
compat_ty({type, 'fun', {type, product, Args1}, Res1},
	  {type, 'fun', {type, product, Args2}, Res2},
	  A, TEnv) ->
    {Ap, Cs} = compat_tys(Args2, Args1, A, TEnv),
    {Aps, Css} = compat(Res1, Res2, Ap, TEnv),
    {Aps, sets:union(Cs, Css)};

% Integer types
compat_ty({type, integer, []}, {type, integer, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, range, _}, {type, integer, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, range, [{integer, I11},{integer, I12}]},
	  {type, range, [{integer, I21},{integer, I22}]},
	  A, _TEnv) when
      I11 >= I21 andalso I12 =< I22 ->
    ret(A);
compat_ty({type, integer, I}, {type, integer, I}, A, _TEnv) ->
    ret(A);
compat_ty({type, integer, _I}, {type, integer, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, integer, I}, {type, range, [{integer,I1},
					     {integer, I2}]}, A, _TEnv)
  when I >= I1 andalso I =< I2 ->
    ret(A);

compat_ty({atom, Atom}, {atom, Atom}, A, _TEnv) ->
    ret(A);

compat_ty({type, float, []}, {type, float, []}, A, _TEnv) ->
    ret(A);

compat_ty({type, bool, []}, {type, bool, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, boolean, []}, {type, bool, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, bool, []}, {type, boolean, []}, A, _TEnv) ->
    ret(A);
compat_ty({type, boolean, []}, {type, boolean, []}, A, _TEnv) ->
    ret(A);
compat_ty({atom, true}, {type, bool, []}, A, _TEnv) ->
    ret(A);
compat_ty({atom, false}, {type, bool, []}, A, _TEnv) ->
    ret(A);
compat_ty({atom, true}, {type, boolean, []}, A, _TEnv) ->
    ret(A);
compat_ty({atom, false}, {type, boolean, []}, A, _TEnv) ->
    ret(A);

compat_ty({type, record, [{atom, Record}]}, {type, record, [{atom, Record}]}, A, _TEnv) ->
    ret(A);

compat_ty({type, list, [Ty1]}, {type, list, [Ty2]}, A, TEnv) ->
    compat(Ty1, Ty2, A, TEnv);
compat_ty({type, nil, []}, {type, list, [_Ty]}, A, _TEnv) ->
    ret(A);

compat_ty({type, tuple, Args1}, {type, tuple, Args2}, A, TEnv) ->
    compat_tys(Args1, Args2, A, TEnv);
compat_ty({user_type, Name, Args}, Ty, A, TEnv) ->
    compat(unfold_user_type(Name, Args, TEnv), Ty, A, TEnv);
compat_ty(Ty, {user_type, Name, Args}, A, TEnv) ->
    compat(Ty, unfold_user_type(Name, Args, TEnv), A, TEnv);

%% Maps
compat_ty({type, map, any}, {type, map, _Assocs}, A, _TEnv) ->
    ret(A);
compat_ty({type, map, _Assocs}, {type, map, any}, A, _TEnv) ->
    ret(A);
compat_ty({type, map, Assocs1}, {type, map, Assocs2}, A, TEnv) ->
    ret(lists:foldl(fun (Assoc2, As) ->
			    any_type(Assoc2, Assocs1, As, TEnv)
		    end, A, Assocs2));
compat_ty({type, AssocTag1, [Key1, Val1]},
	  {type, AssocTag2, [Key2, Val2]}, A, TEnv)
	when AssocTag1 == map_field_assoc, AssocTag2 == map_field_assoc;
	     AssocTag1 == map_field_exact, AssocTag2 == map_field_exact;
	     AssocTag1 == map_field_assoc, AssocTag2 == map_field_exact ->
    %% For M2 <: M1, mandatory fields in M1 must be mandatory fields in M2
    {A1, Cs1} = compat_ty(Key2, Key1, A, TEnv),
    {A2, Cs2} = compat_ty(Val1, Val2, A1, TEnv),
    {A2, sets:union(Cs1, Cs2)};

compat_ty(Ty1, Ty2, _, _) ->
    throw({type_error, compat, 0, Ty1, Ty2}).


compat_tys([], [], A, _TEnv) ->
    ret(A);
compat_tys([Ty1|Tys1], [Ty2|Tys2], A, TEnv) ->
    {Ap, Cs} = compat(Ty1 ,Ty2, A, TEnv),
    {Aps, Css} = compat_tys(Tys1, Tys2, Ap, TEnv),
    {Aps, sets:union(Cs, Css)}.

%% Returns a successful matching of two types. Convenience function for when
%% there were no type variables involved.
ret(A) ->
    {A, sets:new()}.

any_type(_Ty, [], _A, _TEnv) ->
    throw({type_error, no_type_matching});
any_type(Ty, [Ty1|Tys], A, TEnv) ->
    try
	compat_ty(Ty, Ty1, A, TEnv)
    catch
	_ ->
	    any_type(Ty, Tys, A, TEnv)
    end.

%% TODO: Implement user type unfolding
unfold_user_type(_Name, _Args, _TEnv) ->
    unimplemented. %maps:find(Name, TEnv)

remove_pos({atom, _Pos, Atom}) ->
    {atom, Atom};
remove_pos({type, _Pos, Type, Args}) when is_list(Args) ->
    {type, Type, lists:map(fun remove_pos/1, Args)};
remove_pos({type, _Pos, Type, any})->
    {type, Type, any};
remove_pos({user_type, _Pos, Type, Args}) when is_list(Args) ->
    {user_type, Type, lists:map(fun remove_pos/1, Args)};
remove_pos({type, _Pos, 'fun', Args}) ->
    {type, 'fun', remove_pos(Args)};
remove_pos({Type, _Pos, Foo}) ->
    {Type, Foo}.

%%% The environment passed around during typechecking.

-record(env, {fenv   = #{}
	     ,venv   = #{}
	     ,renv   = #{}
	     ,tyenv  = #{}
	     ,tyvenv = #{}
	     }).



%% Arguments: An environment for functions, an environment for variables
%% and the expression to type check.
%% Returns the type of the expression and a collection of variables bound in
%% the expression together with their type.
%-spec type_check_expr(#env, any()) -> { any(), #{ any() => any()} }.
type_check_expr(Env, {var, P, Var}) ->
    case catch maps:get(Var, Env#env.venv) of
	{'EXIT', {{badkey, _}, _}} ->
	    throw({unknown_variable, P, Var});
	Ty ->
	    return(Ty)
    end;
type_check_expr(Env, {match, _, Pat, Expr}) ->
    {Ty, VarBinds} = type_check_expr(Env, Expr),
    {Ty, add_type_pat(Pat, Ty, VarBinds)};
type_check_expr(Env, {'if', _, Clauses}) ->
    infer_clauses(Env, Clauses);
type_check_expr(Env, {'case', _, Expr, Clauses}) ->
    {_ExprTy, VarBinds} = type_check_expr(Env, Expr),
    VEnv = add_var_binds(Env#env.venv, VarBinds),
    infer_clauses(Env#env{ venv = VEnv}, Clauses);
type_check_expr(_Env, {integer, _, _N}) ->
    return({type, 0, any, []});
type_check_expr(Env, {tuple, _, TS}) ->
    { Tys, VarBinds } = lists:unzip([ type_check_expr(Env, Expr)
				    || Expr <- TS ]),
    { {type, 0, tuple, Tys}, union_var_binds(VarBinds) };
type_check_expr(Env, {cons, _, Head, Tail}) ->
    {Ty1, VB1} = type_check_expr(Env, Head),
    {Ty2, VB2} = type_check_expr(Env, Tail),
    % TODO: Should we check the types here?
    case {Ty1, Ty2} of
	{{type, _, any, []}, _} ->
	    {{type, 0, any, []}, VB2};
	{_, {type, _, any, []}} ->
	    {{type, 0, any, []}, VB2};
	{Ty1, TyList = {type, _, list, [Ty]}} ->
	    case subtype(Ty1, Ty) of
		true ->
		    {TyList, union_var_binds([VB1, VB2])};
		false ->
		    throw({type_error, list, 0, Ty1, Ty})
	    end;
	{_, _} ->
	    throw({type_error, list, 0, Ty2})
		% We throw a type error here because Tail is not of type list
		% (nor is it of type any()).
    end;
type_check_expr(Env, {call, P, Name, Args}) ->
    { ArgTys, VarBinds} =
	lists:unzip([ type_check_expr(Env, Arg) || Arg <- Args]),
    VarBind = union_var_binds(VarBinds),
    {FunTy, VarBind2} = type_check_fun(Env, Name, length(Args)),
    case  FunTy of
	{type, _, any, []} ->
	    { {type, 0, any, []}, VarBind };
	[{type, _, 'fun', [{type, _, product, TyArgs}, ResTy]}] ->
	    % TODO: Handle multi-clause function types
	    case subtypes(TyArgs, ArgTys) of
		true ->
		    {ResTy, union_var_binds([VarBind, VarBind2])};
		false ->
		    throw({type_error, call, P, Name, TyArgs, ArgTys})
	    end;
	[{type, _, bounded_fun, [{type, _, 'fun',
				  [{type, _, product, TyArgs}, ResTy]}
				,_Cs]}] ->
	    case subtypes(TyArgs, ArgTys) of
		true ->
		    %%% TODO: Store the constraints
		    {ResTy, union_var_binds([VarBind, VarBind2])};
		false ->
		    throw({type_error, call, P, Name, TyArgs, ArgTys})
	    end
    end;
type_check_expr(Env, {lc, _, Expr, Qualifiers}) ->
    type_check_lc(Env, Expr, Qualifiers);
type_check_expr(Env, {block, _, Block}) ->
    type_check_block(Env, Block);

% Don't return the type of anything other than something
% which ultimately comes from a function type spec.
type_check_expr(_Env, {string, _, _}) ->
    return({type, 0, any, []});
type_check_expr(_Env, {nil, _}) ->
    return({type, 0, any, []});
type_check_expr(_Env, {atom, _, _Atom}) ->
    return({type, 0, any, []});

%% Maps
type_check_expr(Env, {map, _, Assocs}) ->
    type_check_assocs(Env, Assocs);
type_check_expr(Env, {map, _, Expr, Assocs}) ->
    {Ty, VBExpr} = type_check_expr(Env, Expr),
    {Ty, VBAssocs} = type_check_assocs(Env, Assocs),
    % TODO: Update the type of the map.
    % TODO: Check the type of the map.
    {Ty, union_var_binds([VBExpr, VBAssocs])};

%% Records
type_check_expr(Env, {record_field, _P, Expr, Record, {atom, _, Field}}) ->
    {_ExprTy, VB} = type_check_expr_in(Env, {type, 0, record, [{atom, 0, Record}]}, Expr),
    Rec = maps:get(Record, Env#env.renv),
    Ty  = maps:get(Field, Rec),
    {Ty, VB};
type_check_expr(Env, {record, _, Expr, Record, Fields}) ->
    RecTy = {type, 0, record, [{atom, 0, Record}]},
    {_ExprTy, VB1} = type_check_expr_in(Env, RecTy, Expr),
    Rec = maps:get(Record, Env#env.renv),
    VB2 = type_check_fields(Env, Rec, Fields),
    {RecTy, union_var_binds([VB1, VB2])};
type_check_expr(Env, {record, _, Record, Fields}) ->
    RecTy = {type, 0, record, [{atom, 0, Record}]},
    Rec   = maps:get(Record, Env#env.renv),
    VB    = type_check_fields(Env, Rec, Fields),
    {RecTy, VB};

%% Functions
type_check_expr(Env, {'fun', _, {clauses, Clauses}}) ->
    infer_clauses(Env, Clauses);
type_check_expr(Env, {'fun', _, {function, Name, Arity}}) ->
    return(maps:get({Name, Arity}, Env#env.fenv));

type_check_expr(Env, {'receive', _, Clauses}) ->
    infer_clauses(Env, Clauses);

%% Operators
type_check_expr(Env, {op, _, '!', Proc, Val}) ->
    % Message passing is always untyped, which is why we discard the types
    {_, VB1} = type_check_expr(Env, Proc),
    {_, VB2} = type_check_expr(Env, Val),
    {{type, 0, any, []}, union_var_binds([VB1, VB2])};
type_check_expr(Env, {op, P, 'not', Arg}) ->
    {Ty, VB} = type_check_expr(Env, Arg),
    case subtype({type, 0, boolean, []}, Ty) of
	true ->
	    {{type, 0, any, []}, VB};
	false ->
	    throw({type_error, non_boolean_argument_to_not, P, Ty})
    end;
type_check_expr(Env, {op, P, BoolOp, Arg1, Arg2}) when
      (BoolOp == 'andalso') or (BoolOp == 'and') or
      (BoolOp == 'orelse')  or (BoolOp == 'or') ->
    % Bindings from the first argument are only passed along for
    % 'andalso' and 'orelse', not 'and' or 'or'.
    UnionVarBindsSecondArg =
	fun (VB1, VB2) ->
		if (BoolOp == 'and') or (BoolOp == 'or') ->
			VB1;
		   true ->
			union_var_binds([VB1, VB2])
		end
	end,
    case type_check_expr(Env, Arg1) of
	{Ty1, VB1} ->
	    case subtype(Ty1, {type, 0, bool, []}) of
		false ->
		    throw({type_error, boolop, BoolOp, P, Ty1});
		true ->
		    case type_check_expr(Env#env{ venv = UnionVarBindsSecondArg(Env#env.venv,VB1 )}, Arg2) of
			{Ty2, VB2} ->
			    case subtype(Ty2, {type, 0, bool, []}) of
				false ->
				    throw({type_error, boolop, BoolOp, P, Ty1});
				true ->
				    {merge_types([Ty1, Ty2])
				    ,union_var_binds([VB1, VB2])}
			    end
		    end
	    end
    end;
type_check_expr(Env, {op, _, RelOp, Arg1, Arg2}) when
      (RelOp == '=:=') or (RelOp == '==') or
      % It's debatable whether we should allow comparison between any types
      % but right now it's allowed
      (RelOp == '>=')  or (RelOp == '=<') ->
    case {type_check_expr(Env, Arg1)
	 ,type_check_expr(Env, Arg2)} of
	{{Ty1, VB1}, {Ty2, VB2}} ->
	    case subtype(Ty1, Ty2) andalso subtype(Ty2, Ty1) of
		true ->
		    % TODO: Should we return boolean() here in some cases?
		    % If both Ty1 and Ty2 are not any() then one could
		    % plausably return boolean().
		    {{type, 0, any, []}, union_var_binds([VB1, VB2])};
		false ->
		    throw(type_error)
	    end
    end;

%% Exception constructs
%% There is no typechecking of exceptions
type_check_expr(Env, {'catch', _, Arg}) ->
    type_check_expr(Env, Arg);
% TODO: Unclear why there is a list of expressions in try
% This is why: try f(), g() catch _:_ -> x end.
type_check_expr(Env, {'try', _, [Expr], CaseCs, CatchCs, AfterCs}) ->
    {Ty,  VB}  = type_check_expr(Env, Expr),
    Env2 = Env#env{ venv = add_var_binds(VB, Env#env.venv) },
    {TyC, _VB2} = infer_clauses(Env2, CaseCs),
    {TyS, _VB3} = infer_clauses(Env2, CatchCs),
    {TyA, _VB4} = infer_clauses(Env2, AfterCs),
    % TODO: Should we check types against each other instead of
    % just merging them?
    % TODO: Check what variable bindings actually should be propagated
    {merge_types([Ty, TyC, TyS, TyA]), VB}.


type_check_fields(Env, Rec, [{record_field, _, {atom, _, Field}, Expr} | Fields]) ->
    FieldTy = maps:get(Field, Rec),
    {_, VB1} = type_check_expr_in(Env, FieldTy, Expr),
    VB2 = type_check_fields(Env, Rec, Fields),
    union_var_binds([VB1, VB2]);
type_check_fields(_Env, _Rec, []) ->
    #{}.




type_check_lc(Env, Expr, []) ->
    {_Ty, _VB} = type_check_expr(Env, Expr),
    % We're returning any() here because we're in a context that doesn't
    % care what type we return. It's different for type_check_lc_in.
    {{type, 0, any, []}, #{}};
type_check_lc(Env, Expr, [{generate, _, Pat, Gen} | Quals]) ->
    {Ty, _} = type_check_expr(Env, Gen),
    type_check_lc(Env#env{ venv = add_type_pat(Pat, Ty, Env#env.venv) }, Expr, Quals).




type_check_expr_in(Env, {type, _, any, []}, Expr) ->
    type_check_expr(Env, Expr);
type_check_expr_in(Env, Ty, {var, LINE, Var}) ->
    VarTy = maps:get(Var, Env#env.venv),
    case subtype(VarTy, Ty) of
	true ->
	    return(VarTy);
	false ->
	    throw({type_error, tyVar, LINE, Var, VarTy, Ty})
    end;
type_check_expr_in(_Env, Ty, {integer, LINE, _Int}) ->
    case subtype(Ty, {type, 0, integer, []}) of
	true ->
	    return({type, 0, integer, []});
	false ->
	    throw({type_error, int, LINE, Ty})
    end;
type_check_expr_in(_Env, Ty, {float, LINE, _Int}) ->
    case subtype(Ty, {type, 0, float, []}) of
	true ->
	    return({type, 0, float, []});
	false ->
	    throw({type_error, float, LINE, Ty})
    end;
type_check_expr_in(_Env, Ty, Atom = {atom, LINE, _}) ->
    case subtype(Atom, Ty) of
	true ->
	    return(Atom);
	false ->
	    throw({type_error, Atom, LINE, Ty})
    end;
type_check_expr_in(Env, ResTy, {tuple, _LINE, TS}) ->
    case ResTy of
      {type, _, tuple, Tys} ->
	    {ResTys, VarBinds} =
		lists:unzip(
		  lists:map(fun ({Ty, Expr}) -> type_check_expr_in(Env, Ty, Expr)
			    end,
			    lists:zip(Tys,TS))),
	    {{type, 0, tuple, ResTys}, union_var_binds(VarBinds)};
	{type, _, any, []} ->
	    {ResTys, VarBinds} =
		lists:unzip([type_check_expr(Env, Expr) || Expr <- TS]),
	    {{type, 0, tuple, ResTys}, union_var_binds(VarBinds)}
    end;
type_check_expr_in(Env, ResTy, {'case', _, Expr, Clauses}) ->
    {ExprTy, VarBinds} = type_check_expr(Env, Expr),
    Env2 = Env#env{ venv = add_var_binds(Env#env.venv, VarBinds) },
    check_clauses(Env2, ExprTy, ResTy, Clauses);
type_check_expr_in(Env, ResTy, {'if', _, Clauses}) ->
    check_clauses(Env, {type, 0, any, []}, ResTy, Clauses);
type_check_expr_in(Env, ResTy, {call, _, Name, Args}) ->
    {FunTy, VarBinds1} = type_check_fun(Env, Name, length(Args)),
    case FunTy of
	{type, _, any, []} ->
	    {_, VarBinds2} =
		lists:unzip([ type_check_expr(Env, Arg) || Arg <- Args]),

	    VarBind = union_var_binds([VarBinds1 |  VarBinds2]),
	    { {type, 0, any, []}, VarBind };
	[{type, _, 'fun', [{type, _, product, TyArgs}, FunResTy]}] ->
	    % TODO: Handle multi-clause function types
	    {_, VarBinds2} =
		lists:unzip([ type_check_expr_in(Env, TyArg, Arg)
			      || {TyArg, Arg} <- lists:zip(TyArgs, Args) ]),
	    case subtype(ResTy, FunResTy) of
		true ->
		    VarBind = union_var_binds([VarBinds1 | VarBinds2]),
		    {ResTy, VarBind};
		_ ->
		    throw(type_error)
	    end
    end;
type_check_expr_in(Env, ResTy, {'receive', _, Clauses}) ->
    check_clauses(Env, [{type, 0, any, []}], ResTy, Clauses);
type_check_expr_in(Env, ResTy, {op, _, '!', Arg1, Arg2}) ->
    % The first argument should be a pid.
    {_, VarBinds1} = type_check_expr(Env, Arg1),
    {Ty, VarBinds2} = type_check_expr_in(Env, ResTy, Arg2),
    {Ty, union_var_binds([VarBinds1,VarBinds2])};
type_check_expr_in(Env, ResTy, {op, P, 'not', Arg}) ->
    case subtype({type, 0, boolean, []}, ResTy) of
	true ->
	    type_check_expr_in(Env, ResTy, Arg);
	false ->
	    throw({type_error, not_user_with_wrong_type, P, ResTy})
    end;
type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '+' orelse Op == '-' orelse Op == '*' orelse Op == '/' ->
    type_check_arith_op(Env, ResTy, Op, P, Arg1, Arg2);
type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == 'bnot' orelse Op == 'div' orelse Op == 'rem' orelse
      Op == 'band' orelse Op == 'bor' orelse Op == 'bxor' orelse
      Op == 'bsl'  orelse Op == 'bsr' ->
    type_check_int_op(Env, ResTy, Op, P, Arg1, Arg2);
type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == 'and' orelse Op == 'or' orelse Op == 'xor' orelse
      Op == 'andalso' orelse Op == 'orelse' ->
    type_check_logic_op(Env, ResTy, Op, P, Arg1, Arg2);
type_check_expr_in(Env, ResTy, {op, P, Op, Arg1, Arg2}) when
      Op == '++' orelse Op == '--' ->
    type_check_list_op(Env, ResTy, Op, P, Arg1, Arg2)
.

type_check_arith_op(Env, ResTy, Op, P, Arg1, Arg2) ->
    case ResTy of
	{type, _, Ty, []} when Ty == 'integer' orelse Ty == 'float' orelse
			       Ty == 'any' ->
	  {_, VarBinds1} = type_check_expr_in(Env, ResTy, Arg1),
	  {_, VarBinds2} = type_check_expr_in(Env, ResTy, Arg2),
	  {ResTy, union_var_binds([VarBinds1, VarBinds2])};
	_ ->
	  throw({type_error, arith_error, Op, P, ResTy})
    end.
type_check_int_op(Env, ResTy, Op, P, Arg1, Arg2) ->
    case ResTy of
	{type, _, Ty, []} when Ty == 'integer' orelse Ty == 'any' ->
	  {_, VarBinds1} = type_check_expr_in(Env, ResTy, Arg1),
	  {_, VarBinds2} = type_check_expr_in(Env, ResTy, Arg2),
	  {ResTy, union_var_binds([VarBinds1, VarBinds2])};
	_ ->
	  throw({type_error, int_error, Op, P, ResTy})
    end.
type_check_logic_op(Env, ResTy, Op, P, Arg1, Arg2) ->
    case ResTy of
	{type, _, Ty, []} when Ty == 'boolean' orelse Ty == 'bool'
			       orelse Ty == 'any' ->
	  {_, VarBinds1} = type_check_expr_in(Env, ResTy, Arg1),
	  {_, VarBinds2} = type_check_expr_in(Env, ResTy, Arg2),
	  {ResTy, union_var_binds([VarBinds1, VarBinds2])};
	_ ->
	  throw({type_error, logic_error, Op, P, ResTy})
    end.
type_check_list_op(Env, ResTy, Op, P, Arg1, Arg2) ->
    case ResTy of
	{type, _, 'list', [_Ty]} ->
	  {_, VarBinds1} = type_check_expr_in(Env, ResTy, Arg1),
	  {_, VarBinds2} = type_check_expr_in(Env, ResTy, Arg2),
	  {ResTy, union_var_binds([VarBinds1, VarBinds2])};
	{type, _, any, []} ->
	  {_, VarBinds1} = type_check_expr_in(Env, ResTy, Arg1),
	  {_, VarBinds2} = type_check_expr_in(Env, ResTy, Arg2),
	  {ResTy, union_var_binds([VarBinds1, VarBinds2])};
	_ ->
	  throw({type_error, list_op_error, Op, P, ResTy})
    end.


type_check_assocs(Env, [{Assoc, _, Key, Val}| Assocs])
  when Assoc == map_field_assoc orelse Assoc == map_field_exact ->
    {_KeyTy, _KeyVB} = type_check_expr(Env, Key),
    {_ValTy, _ValVB} = type_check_expr(Env, Val),
    % TODO
    type_check_assocs(Env, Assocs);
type_check_assocs(_Env, []) ->
    {{type, 0, any, []}, #{}}.



type_check_fun(Env, {atom, P, Name}, Arity) ->
    % Local function call
    case maps:find({Name, Arity}, Env#env.fenv) of
	{ok, Types} ->
	    {Types, #{}};
	error ->
	    case erl_internal:bif(Name, Arity) of
		true ->
		    {ok, Types} = gradualizer_db:get_spec(erlang, Name, Arity),
		    {Types, #{}};
		false ->
		    throw({call_undef, P, Name, Arity})
	    end
    end;
type_check_fun(_Env, {remote, P, {atom,_,Module}, {atom,_,Fun}}, Arity) ->
    % Module:function call
    case gradualizer_db:get_spec(Module, Fun, Arity) of
	{ok, Types} -> {Types, #{}};
	not_found   -> throw({call_undef, P, Module, Fun, Arity})
    end;
type_check_fun(Env, Expr, _Arity) ->
    type_check_expr(Env, Expr).

type_check_block(Env, [Expr]) ->
    type_check_expr(Env, Expr);
type_check_block(Env, [Expr | Exprs]) ->
    {_, VarBinds} = type_check_expr(Env, Expr),
    type_check_block(Env#env{ venv = add_var_binds(Env#env.venv, VarBinds) }, Exprs).

type_check_block_in(Env, ResTy, [Expr]) ->
    type_check_expr_in(Env, ResTy, Expr);
type_check_block_in(Env, ResTy, [Expr | Exprs]) ->
    {_, VarBinds} = type_check_expr(Env, Expr),
    type_check_block_in(Env#env{ venv = add_var_binds(Env#env.venv, VarBinds) }, ResTy, Exprs).


infer_clauses(Env, Clauses) ->
    {Tys, VarBinds} =
	lists:unzip(lists:map(fun (Clause) ->
				  infer_clause(Env, Clause)
			  end, Clauses)),
    {merge_types(Tys), union_var_binds(VarBinds)}.

infer_clause(Env, {clause, _, Args, Guards, Block}) ->
    EnvNew = Env#env{ venv = add_any_types_pats(Args, Env#env.venv) },
    % TODO: Can there be variable bindings in a guard? Right now we just
    % discard them.
    % TODO: Should we check that guards return boolean()?
    lists:map(fun (GuardConj) ->
		      lists:map(fun (Guard) ->
					type_check_expr(EnvNew, Guard)
				end, GuardConj)
	      end, Guards),
    type_check_block(EnvNew, Block).


check_clauses(Env, ArgsTy, ResTy, Clauses) when
      not is_list(ArgsTy) ->
    check_clauses(Env, [ArgsTy], ResTy, Clauses);
check_clauses(Env, ArgsTy, ResTy, Clauses) ->
    {Tys, VarBinds} =
	lists:unzip(lists:map(fun (Clause) ->
				  check_clause(Env, ArgsTy, ResTy, Clause)
			  end, Clauses)),
    {merge_types(Tys), VarBinds}.

check_clause(Env, ArgsTy, ResTy, {clause, _, Args, Guards, Block}) ->
    case length(ArgsTy) =:= length(Args) of
	false ->
	    throw(argument_length_mismatch);
	true ->
	    EnvNew    = Env#env{ venv = add_types_pats(Args, ArgsTy, Env#env.venv) },
	    VarBinds  = check_guards(EnvNew, Guards),
	    EnvNewest = EnvNew#env{ venv = add_var_binds(EnvNew#env.venv, VarBinds) },
	    type_check_block_in(EnvNewest, ResTy, Block)
    end;
%% DEBUG
check_clause(_Env, _ArgsTy, _ResTy, Term) ->
    io:format("DEBUG: check_clause term: ~p~n", [Term]),
    throw(check_clause).


%% TODO: implement proper checking of guards.
check_guards(Env, Guards) ->
    union_var_binds(
      lists:map(fun (GuardSeq) ->
			union_var_binds(
			  lists:map(fun (Guard) ->
					    begin
						{_Ty, VB} = type_check_expr(Env, Guard), % Do we need to thread the Env?
						VB
					    end
				    end, GuardSeq))
		end, Guards)).

type_check_function(FEnv, REnv, {function,_, Name, NArgs, Clauses}) ->
    case maps:find({Name, NArgs}, FEnv) of
	{ok, [{type, _, 'fun', [{type, _, product, ArgsTy}, ResTy]}]} ->
	    % TODO: Handle multi-clause function types
	    {_, VarBinds} = check_clauses(#env{ fenv = FEnv, renv = REnv },
					  ArgsTy, ResTy, Clauses),
	    {ResTy, VarBinds};
	{ok, {type, _, any, []}} ->
	    infer_clauses(#env{ fenv = FEnv, renv = REnv }, Clauses);
	error ->
	    throw({internal_error, missing_type_spec, Name, NArgs})
    end.

merge_types([]) ->
    {type, 0, any, []};
merge_types([Ty]) ->
    Ty;
merge_types(Tys) ->
    case lists:keyfind(any, 3, Tys) of
	Any = {type, _, any, []} ->
	    Any;
	_ ->
	    case Tys of
		[Ty={atom, _, A}, {atom, _, A} | Rest] ->
		    merge_types([Ty | Rest]);
		[{atom, _, false}, {atom, _, true} | Rest] ->
		    merge_types([{type, 0, boolean, []} | Rest]);
		[{atom, _, true}, {atom, _, false} | Rest] ->
		    merge_types([{type, 0, boolean, []} | Rest]);
		[{atom, _, _}, {type, _, _, _} | _] ->
		    {type, 0, any, []};
		[{type, P, Ty, Args1}, {type, _, Ty, Args2}]
		  when length(Args1) == length(Args2) ->
		    {type, P, Ty, lists:zipwith(fun (A,B) -> merge_types([A,B])
						end, Args1, Args2)};
		[{type, P, tuple, Args1}, {type, _, tuple, Args2} | Rest] ->
		    case length(Args1) == length(Args2) of
			false ->
			    {type, 0, any, []};
			true  ->
			    merge_types([{type, P, tuple,
					  lists:zipwith(fun (A1, A2) ->
								merge_types([A1,A2]) end,
							Args1, Args2)}
					 | Rest])
		    end;
		[{type, _, map, Assocs}, {type, _, map, Assocs}] ->
		    % TODO: Figure out how to merge field assocs properly
		    [{type, 0, map, []}]
	    end
    end.

add_types_pats([], [], VEnv) ->
    VEnv;
add_types_pats([Pat | Pats], [Ty | Tys], VEnv) ->
    add_types_pats(Pats, Tys, add_type_pat(Pat, Ty, VEnv)).

add_type_pat({var, _, '_'}, _Ty, VEnv) ->
    VEnv;
add_type_pat({var, _, A}, Ty, VEnv) ->
    VEnv#{ A => Ty };
add_type_pat({integer, _, _}, _Ty, VEnv) ->
    VEnv;
add_type_pat({tuple, _, Pats}, {type, _, tuple, Tys}, VEnv) when
      length(Pats) == length(Tys) ->
    add_type_pat_list(Pats, Tys, VEnv);
add_type_pat({tuple, _, Pats}, {type, _, any, []}, VEnv) ->
    add_any_types_pats(Pats, VEnv);
add_type_pat({atom, _, Bool}, {type, _, bool, []}, VEnv)
  when Bool == true orelse Bool == false ->
    VEnv;
add_type_pat({atom, _, Bool}, {type, _, boolean, []}, VEnv)
  when Bool == true orelse Bool == false ->
    VEnv;
add_type_pat({atom, _, _}, {type, _, any, []}, VEnv) ->
    VEnv;
add_type_pat({record, _, _Record, Fields}, {type, _, record, [{atom, _, _RecordName}]}, VEnv) ->
    % TODO: We need the definitions of records here, to be able to add the
    % types of the matches in the record.
    add_type_pat_fields(Fields, {type, 0, any, []}, VEnv);
add_type_pat({match, _, Pat1, Pat2}, Ty, VEnv) ->
    add_type_pat(Pat1, Ty, add_type_pat(Pat2, Ty, VEnv)).

add_type_pat_fields([], _, VEnv) ->
    VEnv;
add_type_pat_fields([{record_field, _, _Field, Pat}|Fields], Ty, VEnv) ->
    add_type_pat_fields(Fields, Ty, add_type_pat(Pat, Ty, VEnv)).



add_type_pat_list([Pat|Pats], [Ty|Tys], VEnv) ->
    VEnv2 = add_type_pat(Pat, Ty, VEnv),
    add_type_pat_list(Pats, Tys, VEnv2);
add_type_pat_list([], [], VEnv) ->
    VEnv.


add_any_types_pats([], VEnv) ->
    VEnv;
add_any_types_pats([Pat|Pats], VEnv) ->
    add_any_types_pats(Pats, add_any_types_pat(Pat, VEnv)).

add_any_types_pat(A, VEnv) when is_atom(A) -> % Is this case needed?
    VEnv;
add_any_types_pat({atom, _, _}, VEnv) ->
    VEnv;
add_any_types_pat({integer, _, _}, VEnv) ->
    VEnv;
add_any_types_pat({match, _, P1, P2}, VEnv) ->
    add_any_types_pats([P1, P2], VEnv);
add_any_types_pat({cons, _, Head, Tail}, VEnv) ->
    add_any_types_pats([Head, Tail], VEnv);
add_any_types_pat({nil, _}, VEnv) ->
    VEnv;
add_any_types_pat({tuple, _, Pats}, VEnv) ->
    add_any_types_pats(Pats, VEnv);
add_any_types_pat({var, _,'_'}, VEnv) ->
    VEnv;
add_any_types_pat({var, _,A}, VEnv) ->
    VEnv#{ A => {type, 0, any, []} }.

%%% Helper functions

return(X) ->
    { X, #{} }.

union_var_binds([]) ->
    #{};
union_var_binds([ VarBind | VarBinds ]) ->
    merge(fun glb_types/2, VarBind, union_var_binds(VarBinds)).

add_var_binds(VEnv, VarBinds) ->
    merge(fun glb_types/2, VEnv, VarBinds).

merge(F, M1, M2) ->
    maps:fold(fun (K, V1, M) ->
		 maps:update_with(K, fun (V2) -> F(V1, V2) end, V1, M)
	 end, M2, M1).

% TODO: improve
% Is this the right function to use or should I always just return any()?
glb_types({type, _, N, Args1}, {type, _, N, Args2}) ->
    Args = [ glb_types(Arg1, Arg2) || {Arg1, Arg2} <- lists:zip(Args1, Args2) ],
    {type, 0, N, Args};
glb_types(_, _) ->
    {type, 0, any, []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry pont
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


type_check_file(File) ->
    case gradualizer_db:start_link() of
	{ok, _Pid}                    -> ok;
	{error, {already_started, _}} -> ok
    end,
    {ok, Forms} = epp:parse_file(File,[]),
    #parsedata{specs     = Specs
	      ,functions = Funs
	      ,records   = Records
	      } =
	collect_specs_types_opaques_and_functions(Forms),
    FEnv = create_fenv(Specs, Funs),
    REnv = create_renv(Records),
    lists:foldr(fun (Function, ok) ->
			try type_check_function(FEnv, REnv, Function) of
			    {_Ty, _VarBinds} ->
				ok
			catch
			    Throw ->
				erlang:display(erlang:get_stacktrace()),
				handle_type_error(Throw),
				nok
			end;
		    (_Function, Err) ->
			Err
		end, ok, Funs).

create_renv(Records) ->
    maps:from_list(lists:map(fun ({Rec,Fields}) ->
				     {Rec, maps:from_list(lists:map(fun create_field/1
							     ,Fields))}
			     end, Records)).

create_field({record_field, _, {atom, _, Field}}) ->
    {Field, {type, 0, any, []}};
create_field({record_field, _, {atom, _, Field}, _}) ->
    {Field, {type, 0, any, []}};
create_field({typed_record_field, {record_field, _, {atom, _, Field}}, Ty}) ->
    {Field, Ty};
create_field({typed_record_field, {record_field, _, {atom, _, Field}, _}, Ty}) ->
    {Field, Ty}.


create_fenv(Specs, Funs) ->
% We're taking advantage of the fact that if a key occurrs more than once
% in the list then it right-most occurrence will take precedence. In this
% case it will mean that if there is a spec, then that will take precedence
% over the default type any().
    maps:from_list([ {{Name, NArgs}, {type, 0, any, []}}
		     || {function,_, Name, NArgs, _Clauses} <- Funs
		   ] ++
		   [ {{Name, NArgs}, Types} || {{Name, NArgs}, Types} <- Specs
		   ]
		  ).

%% Collect the top level parse tree stuff returned by epp:parse_file/2.
-spec collect_specs_types_opaques_and_functions(Forms :: list()) -> #parsedata{}.
collect_specs_types_opaques_and_functions(Forms) ->
    aux(Forms, #parsedata{}).

%% Helper for collect_specs_types_opaques_and_functions/1
aux([], Acc) ->
    Acc;
aux([Fun={function, _, _, _, _} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{functions = [Fun | Acc#parsedata.functions]});
aux([{attribute, _, module, Module} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{module = Module});
aux([{attribute, _, spec, Spec} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{specs = [Spec | Acc#parsedata.specs]});
aux([{attribute, _, type, Type} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{types = [Type | Acc#parsedata.types]});
aux([{attribute, _, opaque, Opaque} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{opaques = [Opaque | Acc#parsedata.opaques]});
aux([{attribute, _, record, Record} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{records = [Record | Acc#parsedata.records]});
aux([{attribute, _, export, Exports} | Forms], Acc) ->
    aux(Forms, Acc#parsedata{exports = Exports ++ Acc#parsedata.exports});
aux([{attribute, _, compile, CompileOpts} | Forms], Acc) ->
    Acc1 = lists:foldl(fun (export_all, AccAcc) ->
			       AccAcc#parsedata{export_all = true};
			   (_, AccAcc) ->
			       AccAcc
		       end,
		       Acc,
		       CompileOpts),
    aux(Forms, Acc1);
aux([_|Forms], Acc) ->
    aux(Forms, Acc).

handle_type_error({call_undef, LINE, Func, Arity}) ->
    io:format("Call to undefined function ~p/~p on line ~p~n",
              [Func, Arity, LINE]);
handle_type_error({call_undef, LINE, Module, Func, Arity}) ->
    io:format("Call to undefined function ~p:~p/~p on line ~p~n",
              [Module, Func, Arity, LINE]);
handle_type_error({type_error, tyVar, LINE, Var, VarTy, Ty}) ->
    io:format("The variable ~p on line ~p has type ~s "
	      "but is expected to have type ~s~n",
	      [Var, LINE, pp_type(VarTy), pp_type(Ty)]);
handle_type_error({type_error, {atom, _, A}, LINE, Ty}) ->
    io:format("The atom ~p on line ~p does not have type ~p~n",
	      [A, LINE, pp_type(Ty)]);
handle_type_error({type_error, compat, _LINE, Ty1, Ty2}) ->
    io:format("The type ~p is not compatible with type ~p~n"
	     ,[pp_type(Ty1), pp_type(Ty2)]);
handle_type_error({type_error, list, _, Ty1, Ty}) ->
    io:format("The type ~p cannot be an element of a list of type ~p~n",
	      [pp_type(Ty1), pp_type(Ty)]);
handle_type_error({type_error, list, _, Ty}) ->
    io:format("The type ~p on line ~p is not a list type~n",
	      [pp_type(Ty), line_no(Ty)]);
handle_type_error({type_error, call, _P, Name, TyArgs, ArgTys}) ->
    io:format("The function ~p expects arguments of type~n~p~n but is given "
	      "arguments of type~n~p~n",
	      [Name, TyArgs, ArgTys]);
handle_type_error({type_error, boolop, BoolOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-boolean argument "
	      " of type ~p~n", [BoolOp, P, pp_type(Ty)]);
handle_type_error({type_error, arith_error, ArithOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-numeric argument "
	      " of type ~p~n", [ArithOp, P, pp_type(Ty)]);
handle_type_error({type_error, int_error, IntOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-integer argument "
	      " of type ~p~n", [IntOp, P, pp_type(Ty)]);
handle_type_error({type_error, logic_error, LogicOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-boolean argument "
	      " of type ~p~n", [LogicOp, P, pp_type(Ty)]);
handle_type_error({type_error, list_op_error, ListOp, P, Ty}) ->
    io:format("The operator ~p on line ~p is given a non-list argument "
	      " of type ~p~n", [ListOp, P, pp_type(Ty)]);
handle_type_error({unknown_variable, P, Var}) ->
    io:format("Unknown variable ~p on line ~p.~n", [Var, P]);
handle_type_error(type_error) ->
    io:format("TYPE ERROR~n").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec store_interface_file(map(), #parsedata{}) -> ok | {error, term()}.
store_interface_file(FEnv, ParseData) ->
    Filename = io_lib:format("~p.Gr", [ParseData#parsedata.module]),
    FEnv1 = remove_unexported(FEnv, ParseData),
    FileContents = io_lib:format("~p.~n", [FEnv1]),
    file:write_file(Filename, FileContents).

%% Remove unexported function from an FEnv
-spec remove_unexported(map(), #parsedata{}) -> map().
remove_unexported(FEnv, #parsedata{export_all = true}) ->
    FEnv;
remove_unexported(FEnv, #parsedata{exports = Exports}) ->
    maps:filter(fun (Key, _Value) ->
			lists:member(Key, Exports)
		end,
		FEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pp_type(Type) ->
    %% erl_pp can handle type definitions, so wrap Type in a type definition
    %% for a type called 't'. Then keep only the stuff after "::".
    Form = {attribute, 0, type, {t, Type, []}},
    TypeDef = erl_pp:form(Form),
    {match, [S]} = re:run(TypeDef, <<"::\\s*(.*)\\.\\n*">>,
			  [{capture, all_but_first, binary}]),
    S.

line_no(Ty) ->
    element(2,Ty).

-spec gen_partition(integer(), list(tuple()), fun((tuple()) -> {integer(), term()} | false)) ->
			   tuple().

gen_partition(N,List, Fun) ->
    paux(List, Fun, erlang:list_to_tuple(lists:duplicate(N,[]))).
paux([], _Fun, Tuple) ->
    Tuple;
paux([Elem|List], Fun, Tuple) ->
    case Fun(Elem) of
	{I, Item} ->
	    paux(List, Fun, erlang:setelement(I, [Item | erlang:element(I,Tuple)], Tuple));
	false ->
	    paux(List, Fun, Tuple)
    end.
