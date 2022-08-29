-module(gradualizer_prop).

-compile([export_all]).

-include_lib("common_test/include/ct_property_test.hrl").

%-define(debug(Label, Val), begin ct:pal("~ts:~n~p~n", [Label, Val]), Val end).
-define(debug(Label, Val), Val).

%% Timeout value of a single run of a property.
%% If we exceed this value, we've most likely encountered an infinite loop,
%% as it's very unlikely to take this long to typecheck a single file, type, or expression.
-define(PROP_TIMEOUT, timer:seconds(3)).

-define(pp(Ty), typelib:pp_type(Ty)).

%%
%%' Generators
%%

abstract_type() ->
    gradualizer_gen:abstract_type().

abstract_expr() ->
    gradualizer_gen:expr().

abstract_module() ->
    gradualizer_gen:module().

abstract_term() ->
    gradualizer_erlang_abstract_code:term().

%%.
%%' Properties
%%

prop_reduce_type() ->
    ?FORALL(Type, abstract_type(), prop_reduce_type_(Type)).

prop_reduce_type_(Type) ->
    {_, Acc} = typelib:reduce_type(fun (T, true) -> {T, true} end, true, Type),
    Acc.

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
            begin
                {TextEnv, Env} = create_env(typelib:remove_pos(Type), [return_text_env]),
                ?WHENFAIL(ct:pal("~s failed:\n~ts\n\nwith type env:\n~ts\n",
                                 [?FUNCTION_NAME, ?pp(Type), TextEnv]),
                          ?TIMEOUT(?PROP_TIMEOUT, prop_normalize_type_(Type, Env)))
            end).

prop_normalize_type_(Type, Env) ->
    typechecker:normalize(Type, Env),
    %% we're only interested in normalize termination / infinite recursion
    true.

prop_glb() ->
    ?FORALL({Type1, Type2},
            {abstract_type(), abstract_type()},
            begin
                {TextEnv, Env} = create_env(typelib:remove_pos(type_union([Type1, Type2])),
                                            [return_text_env]),
                ?WHENFAIL(ct:pal("~s failed:\n~ts\n~ts\n\nwith type env:\n~ts\n",
                                 [?FUNCTION_NAME, ?pp(Type1), ?pp(Type2), TextEnv]),
                          ?TIMEOUT(?PROP_TIMEOUT, prop_glb_(Type1, Type2, Env)))
            end).

prop_glb_(Type1, Type2, Env) ->
    Type1_ = typechecker:normalize(typelib:remove_pos(Type1), Env),
    Type2_ = typechecker:normalize(typelib:remove_pos(Type2), Env),
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
            begin
                {TextEnv, Env} = create_env(typelib:remove_pos(type_union([Type1, Type2])),
                                            [return_text_env]),
                ?WHENFAIL(ct:pal("~s failed:\n~ts\n~ts\n\nwith type env:\n~ts\n",
                                 [?FUNCTION_NAME, ?pp(Type1), ?pp(Type2), TextEnv]),
                          ?TIMEOUT(?PROP_TIMEOUT, prop_type_diff_(Type1, Type2, Env)))
            end).

prop_type_diff_(Type1, Type2, Env) ->
    Type1_ = typelib:remove_pos(Type1),
    Type2_ = typelib:remove_pos(Type2),
    typechecker:type_diff(Type1_, Type2_, Env),
    %% we're only interested in termination / infinite recursion
    true.

%% TODO: Actually, directly checking refinable might not be necessary in the long run,
%%       since it's called from type_check_expr_in that we should have a prop for anyway.
prop_refinable() ->
    ?FORALL(Type,
            abstract_type(),
            begin
                {TextEnv, Env} = create_env(typelib:remove_pos(Type), [return_text_env]),
                ?WHENFAIL(ct:pal("~s failed:\n~ts\n\nwith type env:\n~ts\n",
                                 [?FUNCTION_NAME, ?pp(Type), TextEnv]),
                          ?TIMEOUT(?PROP_TIMEOUT, prop_refinable_(Type, Env)))
            end).

prop_refinable_(Type, Env) ->
    Type_ = typelib:remove_pos(Type),
    typechecker:refinable(Type_, Env),
    %% we're only interested in termination / infinite recursion
    true.

prop_compatible() ->
    ?FORALL({Type1, Type2},
            {abstract_type(), abstract_type()},
            begin
                {TextEnv, Env} = create_env(typelib:remove_pos(type_union([Type1, Type2])),
                                            [return_text_env]),
                ?WHENFAIL(ct:pal("~s failed:\n~ts\n~ts\n\nwith type env:\n~ts\n",
                                 [?FUNCTION_NAME, ?pp(Type1), ?pp(Type2), TextEnv]),
                          ?TIMEOUT(?PROP_TIMEOUT, prop_compatible_(Type1, Type2, Env)))
            end).

prop_compatible_(Type1, Type2, Env) ->
    Type1_ = typelib:remove_pos(Type1),
    Type2_ = typelib:remove_pos(Type2),
    typechecker:compatible(Type1_, Type2_, Env),
    %% we're only interested in termination / infinite recursion
    true.

prop_type_check_expr() ->
    %% TODO: use abstract_term() for now, since abstract_expr() gives very unpredictable
    %% and problematic nestings of exprs, e.g. maps inside binaries o_O
    ?FORALL(Expr, abstract_term(),
            ?TIMEOUT(?PROP_TIMEOUT,
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
            ?TIMEOUT(?PROP_TIMEOUT,
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
    ?FORALL(Forms, abstract_module(),
            ?TIMEOUT(?PROP_TIMEOUT,
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

%%.
%%' Helpers
%%

create_env(Type, Opts) ->
    EnvExpr = create_type_env_expr(Type),
    ?debug("env expr", lists:flatten(EnvExpr)),
    Env = test_lib:create_env(EnvExpr, Opts),
    ?debug("env", Env),
    case proplists:get_bool(return_text_env, Opts) of
        false -> Env;
        true -> {EnvExpr, Env}
    end.

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
create_type_env_expr(Type) ->
    {_, Defs} = typelib:reduce_type(fun find_user_types/2, #{}, Type),
    [ case TA of
          {_, 0} ->
              %% we could be more creative here, but there's likely no need to
              Body = typelib:parse_type("{}"),
              declare_type_with_body(Ty, Body);
          _ ->
              Body = type_union(B),
              declare_type_with_body(Ty, Body)
      end
      || {TA, {user_type, _, _, B} = Ty} <- maps:to_list(Defs) ].

find_user_types({user_type, _Anno, Name, Args} = Ty, Defs) ->
    TA = {Name, length(Args)},
    case maps:is_key(TA, Defs) of
        true -> {Ty, Defs};
        false -> {Ty, Defs#{TA => Ty}}
    end;
find_user_types(Ty, Acc) ->
    {Ty, Acc}.

declare_type_with_body({user_type, _, Name, Args}, TyBody) ->
    ArgsSeq = string:join([ ["A", integer_to_list(I)] || I <- lists:seq(1, length(Args))], ", "),
    ArgsAlt = string:join([ ["A", integer_to_list(I)] || I <- lists:seq(1, length(Args))], " | "),
    io_lib:format("-type ~ts(~ts) :: ~ts~ts.~n",
                  [Name, ArgsSeq, typelib:pp_type(TyBody), [ [" | ", ArgsAlt] || Args /= []] ]).

type_union(Tys) ->
    {type, 0, union, Tys}.

%%.
%% vim: foldmethod=marker foldmarker=%%',%%.
