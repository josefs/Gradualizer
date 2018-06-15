-module(test).

-compile([export_all]).

run_tests() ->
    should_pass(),
    should_fail().

should_pass() ->
    {ok, Files} = file:list_dir("tests/should_pass"),
    Fs = lists:map(fun (File) -> "tests/should_pass/" ++ File end, Files),
    lists:foreach(fun typechecker:type_check_file/1, Fs).

should_fail() ->
    {ok, Files} = file:list_dir("tests/should_fail"),
    Fs = lists:map(fun (File) -> "tests/should_fail/" ++ File end, Files),
    lists:foreach(fun (File) ->
      case typechecker:type_check_file(File) of
	nok ->
	      ok;
	ok ->
	      io:format("Typechecking of ~s succeeded but was expected to fail.~n", File),
	      nok
        end
      end,
      Fs).
