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
    Opts = application:get_env(gradualizer, options, []),
    set_union_size_limit(Opts),
    gradualizer_sup:start_link(Opts).

stop(_State) ->
    ok.

set_union_size_limit(Opts) ->
    case lists:keyfind(union_size_limit, 1, Opts) of
        false -> ok;
        {union_size_limit, L} ->
            persistent_term:put(gradualizer_union_size_limit, L)
    end.
