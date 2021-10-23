-module(test_lib).

-export([create_env/1, create_env/2,
         ensure_form_list/1]).

create_env(Opts) ->
    create_env("", Opts).

create_env(String, Opts) ->
    Forms = ensure_form_list(merl:quote(String)),
    ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
    typechecker:create_env(ParseData, Opts).

ensure_form_list(List) when is_list(List) ->
    List;
ensure_form_list(Other) ->
    [Other].
