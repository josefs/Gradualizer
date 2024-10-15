-module(gradualizer_dynamic_suite).

-export([reload/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

reload(Config) ->
    Module = ?config(dynamic_suite_module, Config),
    ?assert(Module /= undefined),
    case erlang:function_exported(Module, generated_tests, 0) of
        true ->
            {ok, Module:generated_tests()};
        false ->
            Path = ?config(dynamic_suite_test_path, Config),
            ?assert(Path /= undefined),
            Forms = get_forms(Module),
            FilesForms = map_erl_files(fun (File) ->
                                               make_test_form(Forms, File, Config)
                                       end, Path),
            {TestFiles, TestForms} = lists:unzip(FilesForms),
            TestNames = [ list_to_atom(filename:basename(File, ".erl")) || File <- TestFiles ],
            ct:pal("All tests found under ~s:\n~p\n", [Path, TestNames]),
            GeneratedTestsForm = make_generated_tests_form(TestNames),
            NewForms = Forms ++ TestForms ++ [GeneratedTestsForm, {eof, 0}],
            {ok, _} = merl:compile_and_load(NewForms),
            {ok, TestNames}
    end.

map_erl_files(Fun, Dir) ->
    Files = filelib:wildcard(filename:join(Dir, "*.erl")),
    [{filename:basename(File), Fun(File)} || File <- Files].

make_test_form(Forms, File, Config) ->
    TestTemplateName = ?config(dynamic_test_template, Config),
    ?assert(TestTemplateName /= undefined),
    TestTemplate = merl:quote("'@Name'(_) -> _@Body."),
    {function, _Anno, _Name, 1, Clauses} = lists:keyfind(TestTemplateName, 3, Forms),
    [{clause, _, _Args, _Guards, ClauseBodyTemplate}] = Clauses,
    TestName = filename:basename(File, ".erl"),
    ClauseBody = merl:subst(ClauseBodyTemplate, [{'File', erl_syntax:string(File)}]),
    TestEnv = [
               {'Name', erl_syntax:atom(TestName)},
               {'Body', ClauseBody}
              ],
    erl_syntax:revert(merl:subst(TestTemplate, TestEnv)).

make_generated_tests_form(TestNames) ->
    Template = merl:quote("generated_tests() -> _@Body."),
    erl_syntax:revert(merl:subst(Template, [{'Body', merl:term(TestNames)}])).

get_forms(Module) ->
    ModPath = code:which(Module),
    {ok, {Module, [Abst]}} = beam_lib:chunks(ModPath, [abstract_code]),
    {abstract_code, {raw_abstract_v1, Forms}} = Abst,
    StripEnd = fun
                   ({eof, _}) -> false;
                   (_) -> true
               end,
    lists:filter(StripEnd, Forms).
