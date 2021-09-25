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
    Opts = application:get_env(gradualizer, cli_options, []),
    gradualizer_sup:start_link(Opts).

stop(_State) ->
    ok.
