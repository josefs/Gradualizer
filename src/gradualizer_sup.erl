%%%-------------------------------------------------------------------
%%% @doc Main Gradualizer supervisor
%%%-------------------------------------------------------------------
-module(gradualizer_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%===================================================================
%% API functions
%%===================================================================

%% @doc Start the supervisor
-spec start_link(Opts) -> R when
      Opts :: list(),
      R :: {ok, Pid :: pid()}
         | {error, {already_started, Pid :: pid()}}
         | {error, {shutdown, term()}}
         | {error, term()}
         | ignore.
start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

%%===================================================================
%% Supervisor callbacks
%%===================================================================

-spec init(Args :: term()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init([Opts]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    Children = [child(gradualizer_db, Opts),
                child(gradualizer_cache, Opts)],
    {ok, {SupFlags, Children}}.

%%===================================================================
%% Internal functions
%%===================================================================

child(Module, Opts) ->
    #{id => Module,
      start => {Module, start_link, [Opts]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [Module]}.


