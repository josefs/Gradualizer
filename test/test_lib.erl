-module(test_lib).

-export([create_env/1, create_env/2,
         ensure_form_list/1]).

create_env(Opts) ->
    create_env("", Opts).

%% @doc Create a `typechecker:env()' from a string containing `-type ...'
%% or `-record(...)' definitions.
%%
%% Usage:
%%
%% 1> test_lib:create_env("-record(r, {f}).", []).
%% {env,#{},#{},#{},
%%      #{module => undefined,
%%        records =>
%%            #{r =>
%%                  [{typed_record_field,{record_field,1,
%%                                                     {atom,1,f},
%%                                                     {atom,1,undefined}},
%%                                       {type,0,any,[]}}]},
%%        types => #{}},
%%      false,false,true}
create_env(TypeEnvString, Opts) ->
    Forms = ensure_form_list(merl:quote(lists:flatten(TypeEnvString))),
    ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
    typechecker:create_env(ParseData, Opts).

ensure_form_list(List) when is_list(List) ->
    List;
ensure_form_list(Other) ->
    [Other].
