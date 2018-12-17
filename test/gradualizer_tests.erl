-module(gradualizer_tests).

-include_lib("eunit/include/eunit.hrl").

api_test_() ->
    [?_assertEqual([], gradualizer:type_check_file("test/should_pass/any.erl")),
     % TODO: Test fixture is not meant to depend on the build results
     ?_assertEqual([], gradualizer:type_check_file("_build/test/lib/gradualizer/test/any.beam")),
     fun() ->
             {module, Mod} = code:load_abs("_build/test/lib/gradualizer/test/any"),
             ?assertEqual([], gradualizer:type_check_module(Mod))
     end,
     fun() ->
             %% user_types.erl references remote_types.erl
             %% it is not in the sourcemap of the DB so let's import it manually
             gradualizer_db:import_erl_files(["test/should_pass/user_types.erl"]),
             ?_assertEqual([], gradualizer:type_check_dir("test/should_pass/"))
     end,

     %% Failure cases
     {"Not found",
      [?_assertThrow({file_not_found, "test/not_found.erl"},
                     gradualizer:type_check_file("test/not_found.erl")),
       ?_assertThrow({file_not_found, "test/not_found.beam"},
                     gradualizer:type_check_file("test/not_found.beam")),
       ?_assertThrow({beam_not_found, non_existing},
                     gradualizer:type_check_module(not_found)),
       ?_assertThrow({dir_not_found, "test/not_found/"},
                     gradualizer:type_check_dir("test/not_found/"))
      ]},
     {setup,
      fun() -> file:write_file("test/bad_content.beam", "bad content") end,
      fun(_) -> file:delete("test/bad_content.beam") end,
      ?_assertThrow({forms_error,{not_a_beam_file, 'test/bad_content.beam'}},
                    gradualizer:type_check_file("test/bad_content.beam"))},
     {setup,
      fun() ->
              {ok, any} = compile:file("test/should_pass/any.erl",
                                       [{outdir, "test/should_pass/"}])
      end,
      fun(_) -> file:delete("test/should_pass/any.beam") end,
      ?_assertThrow({forms_not_found, "test/should_pass/any.beam"},
                    gradualizer:type_check_file("test/should_pass/any.beam"))},
     ?_assertThrow({unknown_file_extension, ".bad_ext"},
                   gradualizer:type_check_file("test/not_found.bad_ext")),
     ?_assertThrow({beam_not_found, preloaded},
                   gradualizer:type_check_module(erlang))
    ].
