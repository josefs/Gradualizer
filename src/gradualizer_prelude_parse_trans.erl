%% @private
-module(gradualizer_prelude_parse_trans).

%% This file is compiled first and used for compiling other files,
%% so no dependencies to other modules within the application.

-export([parse_transform/2, get_module_forms_tuples/1]).

-type form() :: erl_parse:abstract_form() | {error, _} | {eof, _}.
-type forms() :: [form()].

-include("gradualizer.hrl").

parse_transform(Forms, _Options) ->
    replace_get_modules_and_forms(Forms).

%% Replaces the function body of get_modules_and_forms/0
replace_get_modules_and_forms([{function, Anno, get_modules_and_forms, 0, _OldBody} | RestForms]) ->
    PreludeDir = filename:join([filename:dirname(?FILE), "..", "priv", "prelude"]),
    ModuleFormsTuples = get_module_forms_tuples(PreludeDir),
    BodyClauses = [{clause, Anno, [], [],
                    [erl_anno_replace(erl_parse:abstract(ModuleFormsTuples))]}],
    [{function, Anno, get_modules_and_forms, 0, BodyClauses} | RestForms];
replace_get_modules_and_forms([Form | RestForms]) ->
    [Form | replace_get_modules_and_forms(RestForms)].

-spec get_module_forms_tuples(file:filename()) -> [{module(), forms()}].
get_module_forms_tuples(Dir) ->
    Pattern = filename:join([Dir, "*.specs.erl"]),
    Files = filelib:wildcard(Pattern),
    lists:map(fun get_module_and_forms/1, Files).

%% Parses and returns the forms of a file along with the module given in the -module attribute
-spec get_module_and_forms(file:filename()) -> {module(), forms()}.
get_module_and_forms(File) ->
    {ok, Forms} = epp:parse_file(File, []),
    [{attribute, _, file, _},
     {attribute, _, module, Module} | _] = Forms,
    {Module, [ erl_anno_mark(F) || F <- Forms ]}.

erl_anno_mark({eof, _} = Form) ->
    Form;
erl_anno_mark(Form) ->
    Anno = element(2, Form),
    setelement(2, Form, {'$anno', Anno}).

erl_anno_replace(AST) ->
    erl_syntax:revert(erl_syntax_lib:map(fun replace_mark/1, AST)).

replace_mark(T) ->
    case T of
        {tree, tuple, _, [{atom, _, '$anno'}, Anno]} ->
            erl_syntax:application(erl_syntax:atom("erl_anno"), erl_syntax:atom("new"), [Anno]);
        _ ->
            T
    end.
