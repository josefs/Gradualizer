-module(should_pass_SUITE).

-compile([export_all, nowarn_export_all]).

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
    [{timetrap, {minutes, 10}}].

init_per_suite(Config0) ->
    AppBase = code:lib_dir(gradualizer),
    Config = [
              {dynamic_suite_module, ?MODULE},
              {dynamic_suite_test_path, filename:join(AppBase, "test/should_pass")},
              {dynamic_test_template, should_pass_template}
             ] ++ Config0,
    {ok, _} = application:ensure_all_started(gradualizer),
    ok = load_prerequisites(AppBase),
    {ok, TestNames} = gradualizer_dynamic_suite:reload(Config),
    case all() of
        TestNames -> ok;
        _ -> ct:fail("Please update all/0 to list all tests")
    end,
    Config.

load_prerequisites(AppBase) ->
    %% user_types.erl is referenced by remote_types.erl and opaque.erl.
    %% It is not in the sourcemap of the DB so let's import it manually
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_pass/user_types.erl")]),
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_pass/other_module.erl")]),
    %% imported.erl references any.erl
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_pass/any.erl")]),
    ok.

end_per_suite(_Config) ->
    ok = application:stop(gradualizer),
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
    [alias_in_pattern,andalso_any,ann_types,annotated_types,any,
     any_doesnt_have_type_none_pass,any_pattern,bc_pass,
     binary_exhaustiveness_checking,binary_in_union,binary_literal_pattern,
     bitstring,block_scope,bool,bounded_funs,
     call_intersection_function_with_union_arg_pass,'case',
     case_of_record_with_user_defined,catch_expr_pass,covariant_map_keys_pass,
     cyclic_otp_specs,erlang_error_args_none_pass,exhaustiveness_union_types,
     factorial,float,flow,fun_capture,fun_spec,guard,guard_sequences_pass,if_expr,
     imported,int,intersection_pass,intersection_with_any_pass,iodata,issue131,lc,
     lc_generator_not_none,lc_var_binds_in_filters,list,list_concat_op_pass,
     list_exhaustiveness_checking_regressions,
     list_exhaustiveness_checking_regressions2,
     list_exhaustiveness_checking_unreachable_clause_regression,list_infer_pass,
     list_op_pass,listsspecs,map,map_as_argument_update,map_creation,
     map_field_valid_update,map_infer_pass,map_passing_expr,map_passing_subtyping,
     map_pattern,map_refinement,map_update,map_update_with_record_field,
     messaging_pass,minimised_gradualizer_fmt,minus,module_info_higher_arity,
     module_info_pass,named_fun_infer_pass,named_fun_pass,negate_none,
     nested_pattern_match,non_neg_plus_pos_is_pos_pass,nonempty_cons,
     nonempty_list_match_in_head_exhaustive,nonempty_string,
     nonexhaustive_record_pattern,opaque,operator_pattern_pass,operator_subtypes,
     other_module,pattern_bind_reuse,pattern_record,pattern_with_ty_vars,
     poly_lists_map_constraints_pass,poly_lists_map_pass,poly_map_pattern,
     poly_pass,poly_pass_infer,poly_pass_no_solve_constraints,
     poly_union_lower_bound_pass,preludes,qlc_test,record_info,record_refinement,
     record_union_pass,record_union_with_any_should_pass,record_var,
     record_wildcard_pass,record_with_user_defined,records,
     recursive_call_with_remote_union_return_type_pass,recursive_types_passing,
     refine_comparison,refine_mismatch_using_guard_bifs,remote_types,
     remote_types_pass,return_fun,rigid_type_variables,rigid_type_variables_pass,
     scope,send_pass,sets_set,shortcut_ops_pass,
     spec_and_fun_clause_intersection_pass,stuff_as_top,'try',try_expr,tuple,
     tuple_union_pass,tuple_union_pat,tuple_union_pattern_pass,type_decl,
     type_pattern,type_refinement_pass,type_variable,type_vars_term,
     typed_record_field_access,unary_negate_union_with_user_type_pass,unary_plus,
     underscore,user_type_in_pattern_body,user_types,var,var_fun,varbind_in_block,
     varbind_in_case,varbind_in_function_head,varbind_in_lc,variable_binding,
     variable_binding_leaks].

should_pass_template(_@File) ->
    ?assertEqual(ok, gradualizer:type_check_file(_@File)).
