-module(gradualizer_tests).

-include_lib("eunit/include/eunit.hrl").

api_test_() ->
    [?_assertEqual(ok, gradualizer:type_check_file("test/should_pass/any.erl")),
     ?_assertEqual(ok, gradualizer:type_check_file("test/any.beam")),
     fun() ->
             {module, Mod} = code:load_abs("test/any"),
             ?assertEqual(ok, gradualizer:type_check_module(Mod))
     end,
     ?_assertEqual(ok, gradualizer:type_check_dir("test/should_pass/")),

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
