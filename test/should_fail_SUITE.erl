-module(should_fail_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

%% EUnit has some handy macros, so let's use it, too
-include_lib("eunit/include/eunit.hrl").

%% Test server callbacks
-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gradualizer),
    AppBase = code:lib_dir(gradualizer),
    ok = load_prerequisites(AppBase),
    {ok, TestNames} = dynamic_suite_reload(?MODULE, AppBase),
    case all() of
        TestNames -> ok;
        _ -> ct:fail("Please update all/0 to list all should_fail tests")
    end,
    Config.

load_prerequisites(AppBase) ->
    %% user_types.erl is referenced by opaque_fail.erl.
    %% It is not in the sourcemap of the DB so let's import it manually
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_pass/user_types.erl")]),
    %% exhaustive_user_type.erl is referenced by exhaustive_remote_user_type.erl
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_fail/exhaustive_user_type.erl")]),
    ok.

dynamic_suite_reload(Module, AppBase) ->
    Forms = get_forms(Module),
    Path = filename:join(AppBase, "test/should_fail"),
    FilesForms = map_erl_files(fun (File) ->
                                       make_test_form(Forms, File)
                               end, Path),
    {TestFiles, TestForms} = lists:unzip(FilesForms),
    TestNames = [ list_to_atom(filename:basename(File, ".erl")) || File <- TestFiles ],
    ct:pal("All tests found under ~s:\n~p\n", [Path, TestNames]),
    NewForms = Forms ++ TestForms ++ [{eof, 0}],
    {ok, _} = merl:compile_and_load(NewForms),
    {ok, TestNames}.

map_erl_files(Fun, Dir) ->
    Files = filelib:wildcard(filename:join(Dir, "*.erl")),
    [{filename:basename(File), Fun(File)} || File <- Files].

make_test_form(Forms, File) ->
    TestTemplate = merl:quote("'@Name'(_) -> _@Body."),
    {function, _Anno, _Name, 1, Clauses} = lists:keyfind(should_fail_template, 3, Forms),
    [{clause, _, _Args, _Guards, ClauseBodyTemplate}] = Clauses,
    TestName = filename:basename(File, ".erl"),
    ClauseBody = merl:subst(ClauseBodyTemplate, [{'File', erl_syntax:string(File)}]),
    TestEnv = [
               {'Name', erl_syntax:atom(TestName)},
               {'Body', ClauseBody}
              ],
    erl_syntax:revert(merl:subst(TestTemplate, TestEnv)).

should_fail_template(_@File) ->
    Errors = gradualizer:type_check_file(_@File, [return_errors]),
    Timeouts = [ E || {_File, {form_check_timeout, _}} = E <- Errors],
    ?assertEqual(0, length(Timeouts)),
    %% Test that error formatting doesn't crash
    Opts = [{fmt_location, brief},
            {fmt_expr_fun, fun erl_prettypr:format/1}],
    lists:foreach(fun({_, Error}) -> gradualizer_fmt:handle_type_error(Error, Opts) end, Errors),
    {ok, Forms} = gradualizer_file_utils:get_forms_from_erl(_@File, []),
    ExpectedErrors = typechecker:number_of_exported_functions(Forms),
    ?assertEqual(ExpectedErrors, length(Errors)).

get_forms(Module) ->
    ModPath = code:which(Module),
    {ok, {Module, [Abst]}} = beam_lib:chunks(ModPath, [abstract_code]),
    {abstract_code, {raw_abstract_v1, Forms}} = Abst,
    StripEnd = fun
                   ({eof, _}) -> false;
                   (_) -> true
               end,
    lists:filter(StripEnd, Forms).

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [annotated_types_fail,arg,arith_op_fail,arity_mismatch,
     bc_fail,bin_expression,bin_type_error,branch,branch2,call,
     call_intersection_function_with_union_arg_fail,case_pattern,
     case_pattern2,catch_expr_fail,cons,covariant_map_keys_fail,
     cyclic_type_vars,depth,exhaustive,exhaustive_float,
     exhaustive_list_variants,exhaustive_refinable_map_variants,
     exhaustive_remote_user_type,exhaustive_string_variants,
     exhaustive_type,exhaustive_user_type,
     exhaustiveness_check_toggling,generator,guard_fail,
     imported_undef,infer_enabled,intersection_check,
     intersection_fail,intersection_infer,
     intersection_with_any_fail,iodata_fail,lambda_not_fun,
     lc_generator_not_none_fail,lc_not_list,list_infer_fail,
     list_op,list_op_should_fail,list_union_fail,
     lists_map_nonempty_fail,literal_char,literal_patterns,
     logic_op,map_entry,map_fail,map_failing_expr,
     map_failing_subtyping,map_field_invalid_update,map_literal,
     map_pattern_fail,map_refinement_fail,map_type_error,match,
     messaging_fail,module_info_fail,named_fun_fail,
     named_fun_infer_fail,nil,no_idempotent_xor,
     non_neg_plus_pos_is_pos_fail,
     nonempty_list_match_in_head_nonexhaustive,
     nonempty_string_fail,opaque_fail,operator_pattern_fail,
     pattern,pattern_record_fail,poly_fail,poly_lists_map_fail,
     poly_union_lower_bound_fail,pp_intersection,record,
     record_exhaustive,record_field,record_index,
     record_info_fail,record_refinement_fail,record_update,
     record_wildcard_fail,recursive_type_fail,
     recursive_types_failing,rel_op,return_fun_fail,
     rigid_type_variables_fail,send_fail,shortcut_ops_fail,
     spec_and_fun_clause_intersection_fail,string_literal,
     tuple_union_arg_fail,tuple_union_fail,tuple_union_pattern,
     tuple_union_refinement,type_refinement_fail,unary_op,
     unary_plus_fail,union_with_any,unreachable_after_refinement].
