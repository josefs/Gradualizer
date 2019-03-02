%%%-------------------------------------------------------------------
%%% @doc Cache tables for memoizing certain calculated values
%%% Currently cached
%%% - glb(Type1, Type2, Module)
%%% @end
%%%-------------------------------------------------------------------
-module(gradualizer_cache).

-behaviour(gen_server).

%% API
-export([start_link/0,
         ensure_started/0,
         stop/0,
         get_glb/3,
         store_glb/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(GLB_CACHE, gradualizer_glb_cache).

-record(state, {}).

%% give shorter alias
-type type() :: gradualizer_type:abstract_type().

%%===================================================================
%% API
%%===================================================================

-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec ensure_started() -> ok | {error, Error :: any()}.
ensure_started() ->
    case start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        Error -> Error
    end.

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%%
%% GLB Cache
%%

-spec get_glb(module(), type(), type()) -> false | {type(), constraints:constraints()}.
get_glb(Module, T1, T2) ->
    try ets:lookup(?GLB_CACHE, {Module, T1, T2}) of
        [] ->
            false;
        [{_, TyCs}] ->
            TyCs
    catch error:badarg ->
            %% cache not initialized
            false
    end.

-spec store_glb(module(), type(), type(), {type(), constraints:constraints()}) -> ok.
store_glb(Module, T1, T2, TyCs) ->
    try
        ets:insert(?GLB_CACHE, {{Module, T1, T2}, TyCs}),
        ok
    catch error:badarg ->
            %% cache not initialized
            ok
    end.

%%===================================================================
%% gen_server callbacks
%%===================================================================

init([]) ->
    ets:new(?GLB_CACHE, [set, public, named_table]),
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
