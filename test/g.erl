%% @doc Shell utilities for working with Gradualizer.
%%
%% Run the following to get them loaded automatically:
%%
%% ```
%% rebar3 as test shell
%% '''
%%
%% Some examples:
%%
%% ```
%% > g:type_of("[ a || _ <- lists:seq(1, 5) ]").
%% {type,0,list,[{atom,0,a}]}
%% > typelib:pp_type(v(-1)).
%% "[a]"
%% > typechecker:normalize(g:type("a()"), g:env("-type a() :: integer().", [])).
%% {type,0,integer,[]}
%% > typelib:pp_type(v(-1)).
%% "integer()"
%% > g:type_of("fun (A) -> #{tag => my_map, list_of_as => [ A || _ <- lists:seq(1, 5) ]} end").
%% {type,0,'fun',
%%       [{type,0,product,[{type,0,any,[]}]},
%%        {type,0,map,
%%              [{type,0,map_field_assoc,[{atom,0,tag},{atom,0,my_map}]},
%%               {type,0,map_field_assoc,
%%                     [{atom,0,list_of_as},{type,0,list,[{type,0,any,[]}]}]}]}]}
%% > typelib:pp_type(v(-1)).
%% "fun((any()) -> #{tag => my_map, list_of_as => [any()]})"
%% '''
%%
%% A more advanced use case would be coupling this with `gradualizer_tracer' to inspect or debug the
%% inner workings of the type checker live.
%% Watch out for the trace sizes - they can grow huge!
%%
%% ```
%% > gradualizer_tracer:start().
%% ok
%% > g:type_of("[a]").
%% {trace,<0.1296.0>,call,
%%        {typechecker,type_check_expr,[{venv,#{}},{cons,1,{atom,1,a},{nil,1}}]}}
%% {trace,<0.1296.0>,call,{typechecker,type_check_expr,[{venv,#{}},{atom,1,a}]}}
%% {trace,<0.1296.0>,return_from,
%%        {typechecker,type_check_expr,2},
%%        {{atom,0,a},{venv,#{}},{constraints,#{},#{},#{}}}}
%% {trace,<0.1296.0>,return_to,{typechecker,do_type_check_expr,2}}
%% {trace,<0.1296.0>,call,{typechecker,type_check_expr,[{venv,#{}},{nil,1}]}}
%% {trace,<0.1296.0>,return_from,
%%        {typechecker,type_check_expr,2},
%%        {{type,0,nil,[]},{venv,#{}},{constraints,#{},#{},#{}}}}
%% {trace,<0.1296.0>,return_to,{typechecker,do_type_check_expr,2}}
%% {trace,<0.1296.0>,return_from,
%%        {typechecker,type_check_expr,2},
%%        {{type,0,nonempty_list,[{atom,0,a}]},
%%         {venv,#{}},
%%         {constraints,#{},#{},#{}}}}
%% {trace,<0.1296.0>,return_to,{g,type_of,2}}
%% {type,0,nonempty_list,[{atom,0,a}]}
%% '''
%% @end

-module(g).

-compile([export_all, nowarn_export_all]).

-include("typechecker.hrl").

%% @doc Return a Gradualizer type for the passed in Erlang type definition.
-spec type(string()) -> typechecker:type().
type(Type) ->
    typelib:remove_pos(typelib:parse_type(Type)).

%% @see env/2
-spec env() -> typechecker:env().
env() ->
    env([]).

%% @see env/2
-spec env(gradualizer:options()) -> typechecker:env().
env(Opts) ->
    test_lib:create_env(Opts).

%% @doc Create a type checker environment populated by types defined in a source code snippet.
%%
%% ```
%% > rr(typechecker).
%% > g:env("-type a() :: integer().", []).
%% #env{fenv = #{},imported = #{},venv = #{},
%%      tenv = #{module => undefined,records => #{},
%%               types => #{{a,0} => {[],{type,0,integer,[]}}}},
%%      infer = false,verbose = false,exhaust = true,
%%      clauses_stack = [],union_size_limit = 30,
%%      current_spec = none}
%% '''
-spec env(string(), gradualizer:options()) -> typechecker:env().
env(ErlSource, Opts) ->
    test_lib:create_env(ErlSource, Opts).

%% @see type_of/2
-spec type_of(string()) -> typechecker:type().
type_of(Expr) ->
    type_of(Expr, env([infer])).

%% @doc Infer type of an Erlang expression.
%%
%% ```
%% > g:type_of("[ a || _ <- lists:seq(1, 5) ]").
%% {type,0,list,[{atom,0,a}]}
%% '''
-spec type_of(string(), typechecker:env()) -> typechecker:type().
type_of(Expr, Env) ->
    AlwaysInfer = Env#env{infer = true},
    {Ty, _Env, _Cs} = typechecker:type_check_expr(AlwaysInfer, merl:quote(Expr)),
    Ty.
