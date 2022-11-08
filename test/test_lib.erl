-module(test_lib).

-export([create_env_from_file/2,
         create_env/1, create_env/2]).

create_env_from_file(FileName, Opts) ->
    {ok, Data} = file:read_file(FileName),
    gradualizer:env(binary_to_list(Data), Opts).

create_env(Opts) ->
    gradualizer:env(Opts).

create_env(TypeEnvString, Opts) ->
    gradualizer:env(TypeEnvString, Opts).
