%%%-------------------------------------------------------------------
%%% @doc Gradualizer application
%%%-------------------------------------------------------------------
-module(gradualizer_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1
        ]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->

    F = fun
            (Trace, ok) ->
                io:format("~p\n", [Trace])
        end,
    dbg:tracer(process, {F, ok}),
    %dbg:p(all, call),
    %dbg:tpl(typechecker, check_arg_exhaustiveness, x),
    %dbg:tpl(typechecker, exhaustiveness_checking, x),
    %dbg:tpl(typechecker, all_refinable, x),
    %dbg:tpl(typechecker, no_clause_has_guards, x),
    dbg:tpl(typechecker, some_type_not_none, x),
    dbg:tpl(typechecker, type_diff, x),
    dbg:tpl(typechecker, refine, x),

    Opts = application:get_env(gradualizer, options, []),
    gradualizer_sup:start_link(Opts).

stop(_State) ->
    ok.
