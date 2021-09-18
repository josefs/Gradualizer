-module(gradualizer_prelude_parse_trans).

%% This file is compiled first and used for compiling other files,
%% so no dependencies to other modules within the application.

-export([parse_transform/2, get_module_forms_tuples/1]).

-type forms() :: [erl_parse:abstract_form() | {error, _} | {eof, _}].

-include("gradualizer.hrl").

parse_transform(Forms, _Options) ->
    replace_get_modules_and_forms(Forms).

%% Replaces the function body of get_modules_and_forms/0
replace_get_modules_and_forms([{function, Anno, get_modules_and_forms, 0, _OldBody} | RestForms]) ->
    ModuleFormsTuples = get_module_forms_tuples(filename:join([filename:dirname(?FILE), "..", "priv", "prelude"])),
    BodyClauses = [{clause, Anno, [], [],
                    [erl_parse:abstract(ModuleFormsTuples)]}],
    [{function, Anno, get_modules_and_forms, 0, BodyClauses} | RestForms];
replace_get_modules_and_forms([Form | RestForms]) ->
    [Form | replace_get_modules_and_forms(RestForms)].

-spec get_module_forms_tuples(file:name_all()) -> [{module(), forms()}].
get_module_forms_tuples(Dir) ->
    Pattern = ?assert_type(filename:join([Dir, "*.specs.erl"]), file:filename()),
    Files = filelib:wildcard(Pattern),
    lists:map(fun get_module_and_forms/1, Files).

%% Parses and returns the forms of a file along with the module given in the -module attribute
-spec get_module_and_forms(file:filename()) -> {module(), forms()}.
get_module_and_forms(File) ->
    {ok, Forms} = epp:parse_file(File, []),
    [{attribute, _, file, _},
     {attribute, _, module, Module} | _] = Forms,
    {Module, Forms}.
