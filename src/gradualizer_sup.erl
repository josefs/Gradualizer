%%%-------------------------------------------------------------------
%%% @doc Main Gradualizer supervisor
%%%-------------------------------------------------------------------
-module(gradualizer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%===================================================================
%% API functions
%%===================================================================

%% @doc Start the supervisor
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%===================================================================
%% Supervisor callbacks
%%===================================================================

-spec init(Args :: term()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    Children = [child(gradualizer_db),
                child(gradualizer_cache)],
    {ok, {SupFlags, Children}}.

%%===================================================================
%% Internal functions
%%===================================================================

child(Module) ->
    #{id => Module,
      start => {Module, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [Module]}.

    
