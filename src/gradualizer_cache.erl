%% @private
%% @doc Cache tables for memoizing certain calculated values
%% Currently cached
%% - glb(Type1, Type2, Module)
%% @end
-module(gradualizer_cache).

-behaviour(gen_server).

%% API
-export([start_link/1,
         get/2,
         store/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(GLB_CACHE, gradualizer_glb_cache).
-define(SUB_CACHE, gradualizer_sub_cache).

-record(state, {}).

%%===================================================================
%% API
%%===================================================================

-spec start_link(Opts) -> R when
      Opts :: list(),
      R :: {ok, Pid :: pid()}
         | {error, Error :: {already_started, pid()}}
         | {error, Error :: any()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

%%
%% GLB Cache
%%

-spec get(atom(), any()) -> none | {some, any()}.
get(glb, Key) -> get_(?GLB_CACHE, Key);
get(subtype, Key) -> get_(?SUB_CACHE, Key).

get_(Cache, Key) ->
    try ets:lookup(Cache, Key) of
        [] ->
            none;
        [{_, Value}] ->
            {some, Value}
    catch error:badarg ->
        %% cache not initialized
        none
    end.

-spec store(atom(), any(), any()) -> ok.
store(glb, Key, Value) -> store_(?GLB_CACHE, Key, Value);
store(subtype, Key, Value) -> store_(?SUB_CACHE, Key, Value).

store_(Cache, Key, Value) ->
    try
        ets:insert(Cache, {Key, Value}),
        ok
    catch error:badarg ->
        %% cache not initialized
        ok
    end.

%%===================================================================
%% gen_server callbacks
%%===================================================================

init([_Opts]) ->
    ets:new(?GLB_CACHE, [set, public, named_table]),
    ets:new(?SUB_CACHE, [set, public, named_table]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
