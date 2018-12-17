-module(gradualizer_cli).
-export([main/1]).

-spec main([string()]) -> ok.
main([]) ->
    print_usage();
main(Args) -> handle_args(Args).

-spec handle_args([string()]) -> no_return().
handle_args(Args) ->
    {Rest, Opts} = parse_opts(Args, []),
    HasHelp = proplists:get_bool(help, Opts),
    HasVersion = proplists:get_bool(version, Opts),
    Status = if
        HasHelp -> print_usage(), ok;
        HasVersion -> print_version(), ok;
        true -> case gradualizer:type_check_files(Rest, Opts) of
                    [] -> ok;
                    _  -> nok
                end
    end,
    io:format("~p~n", [Status]),
    case Status of
        ok  -> halt(0);
        nok -> halt(1)
    end.

-spec get_ver(atom()) -> string().
get_ver(App) ->
    {_, _, Ver} = lists:keyfind(App, 1, application:loaded_applications()),
    Ver.

print_version() ->
    application:load(gradualizer),
    io:format("Gradualizer v~s~n", [get_ver(gradualizer)]).

print_usage() ->
    io:format("Usage: gradualizer [options] [PATH...]~n"),
    io:format("A type checker for Erlang/Elixir~n~n"),
    io:format("       PATH                      Files or directories to type check~n"),
    io:format("  -h,  --help                    display this help and exit~n"),
    io:format("       --infer                   Infer type information from literals and other~n"),
    io:format("                                 language constructs~n"),
    io:format("       --no-infer                Only use type information from function specs~n"),
    io:format("                                  - the default behaviour~n"),
    io:format("  -pa, --path-add                Add the specified directory to the beginning of~n"),
    io:format("                                 the code path; see erl -pa             [string]~n"),
    io:format("       --print-file              prefix error printouts with the file name the~n"),
    io:format("                                 error is from~n"),
    io:format("       --no-print-file           inverse of --print-file~n"),
    io:format("                                  - the default behaviour~n"),
    io:format("       --stop-on-first-error     stop type checking at the first error~n"),
    io:format("       --no-stop-on-first-error  inverse of --stop-on-first-error~n"),
    io:format("                                  - the default behaviour~n").

-spec parse_opts([string()], typechecker:options()) -> {[string()], typechecker:options()}.
parse_opts([], Opts) ->
    {[], Opts};
parse_opts([A | Args], Opts) ->
    case A of
        "-h"                       -> {[], [help]};
        "--help"                   -> {[], [help]};
        "--infer"                  -> parse_opts(Args, [infer | Opts]);
        "--no-infer"               -> parse_opts(Args, [{infer, false} | Opts]);
        "-pa"                      -> handle_path_add(A, Args, Opts);
        "--path-add"               -> handle_path_add(A, Args, Opts);
        "--print-file"             -> parse_opts(Args, [print_file | Opts]);
        "--no-print-file"          -> parse_opts(Args, [{print_file, false} | Opts]);
        "--stop-on-first-error"    -> parse_opts(Args, [stop_on_first_error | Opts]);
        "--no-stop-on-first-error" -> parse_opts(Args, [{stop_on_first_error, false} | Opts]);
        "--crash-on-error"         -> parse_opts(Args, [crash_on_error | Opts]);
        "--no-crash-on-error"      -> parse_opts(Args, [{no_crash_on_error, false} | Opts]);
        "--version"                -> {[], [version]};
        "--"                       -> {Args, Opts};
        "-" ++ _                   -> erlang:error(string:join(["Unknown parameter:", A], " "));
        _                          -> {[A | Args], Opts}
    end.

-spec handle_path_add(string(), [string()], typechecker:options()) -> {[string()], typechecker:options()}.
handle_path_add(A, [], _) ->
    erlang:error(string:join(["Missing argument for", A], " "));
handle_path_add(A, [Path | Args], Opts) ->
    case code:add_patha(Path) of
        true       -> parse_opts(Args, Opts);
        {error, _} -> erlang:error(string:join(["Bad directory for ", A, ": ", Path], ""))
    end.
