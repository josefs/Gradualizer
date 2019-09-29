-module(gradualizer_prelude_parse_trans).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    replace_get_modules_and_forms(Forms).

%% Replaces the function body of get_modules_and_forms/0
replace_get_modules_and_forms([{function, Anno, get_modules_and_forms, 0, _OldBody} | RestForms]) ->
    ModuleFormsTuples = get_module_forms_tuples(),
    BodyClauses = [{clause, Anno, [], [],
                    [erl_parse:abstract(ModuleFormsTuples)]}],
    [{function, Anno, get_modules_and_forms, 0, BodyClauses} | RestForms];
replace_get_modules_and_forms([Form | RestForms]) ->
    [Form | replace_get_modules_and_forms(RestForms)].

%% The value to be returned by the function in the parse transformed module
-spec get_module_forms_tuples() -> [{module(), gradualizer_file_utils:abstract_forms()}].
get_module_forms_tuples() ->
    Files = filelib:wildcard(filename:join([code:priv_dir(gradualizer), "prelude", "*.erl"])),
    lists:map(fun get_module_and_forms/1, Files).

%% Parses and returns the forms of a file along with the module given in the -module attribute
-spec get_module_and_forms(file:filename()) -> {module(), gradualizer_file_utils:abstract_forms()}.
get_module_and_forms(File) ->
    {ok, Forms} = gradualizer_file_utils:get_forms_from_erl(File, []),
    [{attribute, _, file, _},
     {attribute, _, module, Module} | _] = Forms,
    {Module, Forms}.
