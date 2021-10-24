-module(gradualizer_prop).

-compile([export_all]).

-include_lib("common_test/include/ct_property_test.hrl").

%-define(debug(Label, Val), begin ct:pal("~ts:~n~p~n", [Label, Val]), Val end).
-define(debug(Label, Val), Val).

abstract_type() ->
    gradualizer_type_gen:abstract_type().

abstract_expr() ->
    gradualizer_type_gen:expr().

abstract_module() ->
    gradualizer_type_gen:module().

abstract_term() ->
    gradualizer_erlang_abstract_code:term().

prop_preorder_walk_type() ->
    ?FORALL(Type, abstract_type(),
            prop_preorder_walk_type_(Type)).

prop_preorder_walk_type_(Type) ->
    gradualizer_type:preorder(fun (T, true) -> true end, true, Type).

prop_postorder_walk_type() ->
    ?FORALL(Type, abstract_type(),
            prop_postorder_walk_type_(Type)).

prop_postorder_walk_type_(Type) ->
    gradualizer_type:postorder(fun (T, true) -> true end, true, Type).

prop_remove_pos_removes_pos() ->
    ?FORALL(Type, abstract_type(),
            ?WHENFAIL(ct:pal("~s failed:\n~p\n", [?FUNCTION_NAME, Type]),
                      prop_remove_pos_removes_pos_(Type))).

prop_remove_pos_removes_pos_(Type) ->
    typelib:remove_pos(Type),
    %% we're only interested in termination / infinite recursion for now
    true.

prop_normalize_type() ->
    ?FORALL(Type,
            abstract_type(),
            ?WHENFAIL(ct:pal("~s failed:\n~p\n", [?FUNCTION_NAME, Type]),
                      ?TIMEOUT(timer:seconds(1),
                               prop_normalize_type_(Type)))).

prop_normalize_type_(Type) ->
    Env = create_env(Type, []),
    typechecker:normalize(Type, Env),
    %% we're only interested in normalize termination / infinite recursion
    true.

create_env({user_type, _, _, _} = Type, Opts) ->
    EnvExpr = create_recursive_type_env_expr(Type),
    ?debug("env expr", lists:flatten(EnvExpr)),
    Env = test_lib:create_env(EnvExpr, Opts),
    ?debug("env", Env);
create_env(Type, Opts) ->
    EnvExpr = [ declare_type(UserTy) || UserTy <- gather_user_types(Type) ],
    Env = test_lib:create_env(EnvExpr, Opts),
    ?debug("env", Env).

gather_user_types(Type) ->
    gradualizer_type:preorder(fun
                                  ({user_type, _, _, _} = UserTy, Acc) -> [UserTy | Acc];
                                  (_, Acc) -> Acc
                              end, [], Type).

%% @doc Generate a mutually recursive type env from an example user type.
%%
%% We want to test properties on recursive types, not just on simple types.
%% This function creates an env of mutually recursive types with a somewhat arbitrarily chosen
%% algorithm - the point is to generate complex enough types which might trigger infinite recursion
%% in the typechecker.
%% In other words, we use a generated type instance to generate an environment
%% of possibly recursive types that refer to each other.
%%
%% The algorithm to get to a type env from a single "complex enough" type is roughly this:
%% - find all user types in the input type term by going from top to bottom
%% - user types with no args are defined as an empty tuple,
%%   that is `t1()' generates `-type t1() :: {}.'
%% - user types with args are defined as generic types with number of params equal to the number of
%%   present args and the body equivalent to a union of all actual args and formal params,
%%   that is `t2(t2(t2(t1())) | t2(t1()) | any_atom)'
%%   generates `-type t2(A1) :: t2(t2(t1())) | t2(t1()) | any_atom | A1.'
%%
%% In other words, the following generated type:
%%
%% ```
%% t2(t2(t2(t1())) | t2(t1()) | any_atom)
%% '''
%%
%% Generates a type env equivalent to writing the following type definitions by hand:
%%
%% ```
%% -type t1() :: {}.
%% -type t2(A1) :: t2(t2(t1())) | t2(t1()) | any_atom | A1.
%% '''
create_recursive_type_env_expr({user_type, _, _, _} = Type) ->
    Defs = gradualizer_type:preorder(fun find_user_types/2, #{}, Type),
    [ case TA of
          {_, 0} ->
              Body = typelib:parse_type("{}"), %% we could be more creative here, but there's likely no need to
              declare_type_with_body(Ty, Body);
          _ ->
              Body = [ fix_type_body(B1) || B1 <- B ],
              declare_type_with_body(Ty, Body)
      end
      || {TA, {user_type, _, _, B} = Ty} <- maps:to_list(Defs) ].

find_user_types({user_type, _Anno, Name, Args} = Ty, Defs) ->
    TA = {Name, length(Args)},
    case maps:is_key(TA, Defs) of
        true -> Defs;
        false -> Defs#{TA => Ty}
    end;
find_user_types(_, Acc) ->
    Acc.

%% Some constructs are not allowed in a type definition - let's make sure we don't use any of them.
fix_type_body(Type) ->
    Fix = fun
              ({ann_type, _, [_TyVar, TyDef]}, _) -> TyDef;
              (T, _) -> T
          end,
    gradualizer_type:postorder(Fix, none, Type).

declare_type_with_body({user_type, _, Name, Args} = Ty, TyBody) ->
    ArgsSeq = string:join([ ["A", integer_to_list(I)] || I <- lists:seq(1, length(Args))], ", "),
    ArgsAlt = string:join([ ["A", integer_to_list(I)] || I <- lists:seq(1, length(Args))], " | "),
    io_lib:format("-type ~ts(~ts) :: ~ts~ts.~n",
                  [Name, ArgsSeq, typelib:pp_type(TyBody), [ [" | ", ArgsAlt] || Args /= []] ]).

declare_type({user_type, _, Name, Args} = Ty) ->
    TArgs = string:join([ ["A", integer_to_list(I)] || I <- lists:seq(1, length(Args))], ", "),
    io_lib:format("-type ~ts(~ts) :: {~ts}.\n", [Name, TArgs, TArgs]).

prop_glb() ->
    ?FORALL({Type1, Type2},
            {abstract_type(), abstract_type()},
            ?TIMEOUT(timer:seconds(1),
                     prop_glb_(Type1, Type2))).

prop_glb_(Type1, Type2) ->
    Env = test_lib:create_env([]),
    Type1_ = typechecker:normalize(Type1, Env),
    Type2_ = typechecker:normalize(Type2, Env),
    typechecker:glb(Type1_, Type2_, Env),
    %% we're only interested in termination / infinite recursion
    true.

prop_int_range_to_types() ->
    ?FORALL(Range, gradualizer_int:int_range(),
            prop_int_range_to_types_(Range)).

prop_int_range_to_types_(Range) ->
    Types = gradualizer_int:int_range_to_types(Range),
    lists:all(fun gradualizer_int:is_int_type/1, Types).

prop_int_range_to_types_to_int_range() ->
    ?FORALL(Range, gradualizer_int:int_range(),
            prop_int_range_to_types_to_int_range_(Range)).

prop_int_range_to_types_to_int_range_(Range) ->
    Types = gradualizer_int:int_range_to_types(Range),
    Ranges = [ gradualizer_int:int_type_to_range(Type) || Type <- Types ],
    lists:all(fun is_valid_int_range/1, Ranges).

is_valid_int_range({I, J}) when I =< J -> true;
is_valid_int_range({neg_inf, J}) when is_integer(J) -> true;
is_valid_int_range({I, pos_inf}) when is_integer(I) -> true;
is_valid_int_range(_) -> false.

prop_type_diff() ->
    ?FORALL({Type1, Type2},
            {abstract_type(), abstract_type()},
            ?TIMEOUT(timer:seconds(1),
                     prop_type_diff_(Type1, Type2))).

prop_type_diff_(Type1, Type2) ->
    Env = test_lib:create_env([]),
    typechecker:type_diff(Type1, Type2, Env),
    %% we're only interested in termination / infinite recursion
    true.

%% TODO: Actually, directly checking refinable might not be necessary in the long run,
%%       since it's called from type_check_expr_in that we should have a prop for anyway.
prop_refinable() ->
    ?FORALL(Type,
            {abstract_type(), abstract_type()},
            ?TIMEOUT(timer:seconds(1),
                     prop_refinable_(Type))).

prop_refinable_(Type) ->
    Env = test_lib:create_env([]),
    typechecker:refinable(Type, Env),
    %% we're only interested in termination / infinite recursion
    true.

prop_compatible() ->
    ?FORALL({Type1, Type2},
            {abstract_type(), abstract_type()},
            ?TIMEOUT(timer:seconds(1),
                     prop_compatible_(Type1, Type2))).

prop_compatible_(Type1, Type2) ->
    Env = test_lib:create_env([]),
    Type1_ = typelib:remove_pos(Type1),
    Type2_ = typelib:remove_pos(Type2),
    typechecker:compatible(Type1_, Type2_, Env),
    %% we're only interested in termination / infinite recursion
    true.

prop_type_check_expr() ->
    %% TODO: use abstract_term() for now, since abstract_expr() gives very unpredictable
    %% and problematic nestings of exprs, e.g. maps inside binaries o_O
    ?FORALL(Expr, abstract_term(),
            ?TIMEOUT(timer:seconds(1),
                     prop_type_check_expr_(Expr))).

prop_type_check_expr_(Expr) ->
    Env = test_lib:create_env([]),
    case catch typechecker:type_check_expr(Env, Expr) of
        {'EXIT', Reason} ->
            ct:pal("failed with:\n~p\n~p\n", [Expr, Reason]),
            false;
        _ ->
            true
    end.

prop_type_check_expr_in() ->
    %% TODO: use abstract_term() for now, since abstract_expr() gives very unpredictable
    %% and problematic nestings of exprs, e.g. maps inside binaries o_O
    ?FORALL({Type, Expr}, {abstract_type(), abstract_term()},
            ?TIMEOUT(timer:seconds(1),
                     prop_type_check_expr_in_(Type, Expr))).

prop_type_check_expr_in_(Type, Expr) ->
    Env = test_lib:create_env([]),
    case catch typechecker:type_check_expr_in(Env, Type, Expr) of
        {'EXIT', Reason} ->
            ct:pal("failed with:\n~p\n~p\n", [Expr, Reason]),
            false;
        _ ->
            true
    end.

prop_type_check_forms() ->
    %% TODO: use abstract_term() for now, since abstract_expr() gives very unpredictable
    %% and problematic nestings of exprs, e.g. maps inside binaries o_O
    ?FORALL(Forms, abstract_module(),
            ?TIMEOUT(timer:seconds(2),
                     prop_type_check_forms_(Forms))).

prop_type_check_forms_(Forms) ->
    Opts = [],
    case catch typechecker:type_check_forms(Forms, Opts) of
        {'EXIT', Reason} ->
            ct:pal("failed with:\n~p\n~p\n", [Forms, Reason]),
            false;
        _ ->
            true
    end.

%% TODO: prop_ add_type_pat - ultimately called from type_check_expr_in; requires a pattern() gen
%% TODO: prop_ type_check_forms - this one will actually subsume all of the above if we devise a good
%%       enough generator; requires a form() generator
