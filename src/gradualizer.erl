%%% @doc Main external API of the Gradualizer
%%%
%%% The functions `type_check(file|module|dir)' accept the following options:
%%% - `{i, Dir}': Include path for `-include' and `-include_lib' when checking
%%%   Erlang source files. Specify multiple times for multiple include paths.
%%% - `stop_on_first_error': if `true' stop type checking at the first error,
%%%   if `false' continue checking all functions in the given file and all files
%%%   in the given directory.
%%% - `crash_on_error': if `true' crash on the first produced error
%%% - `return_errors': if `true', turns off error printing and errors
%%%   (in their internal format) are returned in a list instead of being
%%%   condensed into a single ok | nok.
%%% - `fmt_location': how to format location when pretty printing errors
%%%   - `none': no location for easier comparison
%%%   - `brief': for machine processing ("LINE:COLUMN:" before message text)
%%%   - `verbose' (default): for human readers
%%%     ("on line LINE at column COLUMN" within the message text)
%%% - `fmt_expr_fun': function to pretty print an expression AST
%%%   (useful to support other languages)
%%% - `fmt_type_fun': function to pretty print a type AST
%%%   (useful to support other languages)
%%% - `{color, always | never | auto}': Use colors when printing fancy messages.
%%%   Auto is the default but auto-detection of tty doesn't work when running
%%%   as an escript. It works when running from the Erlang shell though.
%%% - `{fancy, boolean()}': Use fancy error messages when possible. True by
%%%   default. Doesn't work when a custom `fmt_expr_fun' is used.
-module(gradualizer).

-export([type_check_file/1,
         type_check_file/2,
         type_check_module/1,
         type_check_module/2,
         type_check_dir/1,
         type_check_dir/2,
         type_check_files/1,
         type_check_files/2,
         type_check_forms/2
        ]).

-export_type([options/0, top/0]).

-type options() :: proplists:proplist().

%% This type is the top of the subtyping lattice.
-opaque top() :: any().

-include("gradualizer.hrl").

%% API functions

%% @doc Type check a source or beam file
-spec type_check_file(file:filename()) -> ok | nok | [{file:filename(), any()}].
type_check_file(File) ->
    type_check_file(File, []).

%% @doc Type check a source or beam file
-spec type_check_file(file:filename(), options()) -> ok | nok | [{file:filename(), any()}].
type_check_file(File, Opts) ->
    case filename:extension(File) of
        ".erl" ->
            Includes = proplists:get_all_values(i, Opts),
            case gradualizer_file_utils:get_forms_from_erl(File, Includes) of
                {ok, Forms} ->
                    lint_and_check_forms(Forms, File, Opts);
                Error ->
                    throw(Error)
            end;
        ".beam" ->
            case gradualizer_file_utils:get_forms_from_beam(File) of
                {ok, Forms} ->
                    type_check_forms(File, Forms, Opts);
                Error ->
                    throw(Error)
            end;
        Ext ->
            throw({unknown_file_extension, Ext})
    end.

%% @doc Runs an erl_lint pass, to check if the forms can be compiled at all,
%% before running the type checker.
-spec lint_and_check_forms(Forms, file:filename(), options()) -> R when
      Forms :: gradualizer_file_utils:abstract_forms(),
      R :: ok | nok | [{file:filename(), any()}].
lint_and_check_forms(Forms, File, Opts) ->
    case erl_lint:module(Forms, File, [return_errors]) of
        {ok, _Warnings} ->
            type_check_forms(File, Forms, Opts);
        {error, Errors, _Warnings} ->
            %% If there are lint errors (i.e. compile errors like undefined
            %% variables) we don't even try to type check.
            case proplists:get_bool(return_errors, Opts) of
                true ->
                    [{Filename, ErrorInfo} || {Filename, ErrorInfos} <- Errors,
                                              ErrorInfo <- ErrorInfos];
                false ->
                    [gradualizer_fmt:print_errors(ErrorInfos,
                                                  [{filename, Filename} | Opts])
                     || {Filename, ErrorInfos} <- Errors],
                    nok
            end
    end.

%% @doc Type check a module
-spec type_check_module(module()) -> ok | nok | [{file:filename(), any()}].
type_check_module(Module) ->
    type_check_module(Module, []).

%% @doc Type check a module
-spec type_check_module(module(), options()) ->
                               ok | nok | [{file:filename(), any()}].
type_check_module(Module, Opts) when is_atom(Module) ->
    case code:which(Module) of
        File when is_list(File) ->
            type_check_file(File, Opts);
        Error when is_atom(Error) ->
            throw({beam_not_found, Error})
    end.

%% @doc Type check all source or beam files in a directory.
-spec type_check_dir(file:filename()) -> ok | nok | [{file:filename(), any()}].
type_check_dir(Dir) ->
    type_check_dir(Dir, []).

%% @doc Type check all source or beam files in a directory.
-spec type_check_dir(file:filename(), options()) ->
                            ok | nok | [{file:filename(), any()}].
type_check_dir(Dir, Opts) ->
    case filelib:is_dir(Dir) of
        true ->
            Pattern = ?assert_type(filename:join(Dir, "*.{erl,beam}"), file:filename()),
            type_check_files(filelib:wildcard(Pattern), Opts);
        false ->
            throw({dir_not_found, Dir})
    end.

%% @doc Type check a source or beam file
-spec type_check_files([file:filename()]) ->
                              ok | nok | [{file:filename(), any()}].
type_check_files(Files) ->
    type_check_files(Files, []).

%% @doc Type check a source or beam
-spec type_check_files([file:filename()], options()) ->
                              ok | nok | [{file:filename(), any()}].
type_check_files(Files, Opts) ->
    StopOnFirstError = proplists:get_bool(stop_on_first_error, Opts),
    ReturnErrors = proplists:get_bool(return_errors, Opts),
    if ReturnErrors ->
            lists:foldl(
              fun(File, Errors) when Errors =:= [];
                                     not StopOnFirstError ->
                      type_check_file_or_dir(File, Opts) ++ Errors;
                 (_, Errors) ->
                      Errors
              end, [], Files);
       true ->
            lists:foldl(
              fun(File, Res) when Res =:= ok;
                                  not StopOnFirstError ->
                      case type_check_file_or_dir(File, Opts) of
                          ok -> Res;
                          nok -> nok
                      end;
                 (_, nok) ->
                      nok
              end, ok, Files)
    end.

-spec type_check_file_or_dir(file:filename(), options()) ->
                                    ok | nok | [{file:filename(), any()}].
type_check_file_or_dir(File, Opts) ->
    IsRegular = filelib:is_regular(File),
    IsDir = filelib:is_dir(File),
    if
        IsDir     -> type_check_dir(File, Opts);
        IsRegular -> type_check_file(File, Opts);
        true      -> throw({file_not_found, File}) % TODO: better errors
    end.

%% @doc Type check an abstract syntax tree of a module. This can be useful
%% for tools where the abstract forms are generated in memory.
%%
%% If the first form is a file attribute (as in forms returned by e.g.
%% epp:parse_file/1,2), that filename will be used in error messages.
%% The second form is typically the module attribute.
-spec type_check_forms([erl_parse:abstract_form()], options()) ->
                            ok | nok | [{file:filename(), any()}].
type_check_forms(Forms, Opts) ->
    File = case Forms of
               [{attribute, _, file, {F,  _}} |  _] -> F;
               _ -> "no filename"
           end,
    type_check_forms(File, Forms, Opts).

%% Helper
-spec type_check_forms(file:filename(), Forms, options()) -> R when
      Forms :: gradualizer_file_utils:abstract_forms(),
      R :: ok | nok | [{file:filename(), any()}].
type_check_forms(File, Forms, Opts) ->
    ReturnErrors = proplists:get_bool(return_errors, Opts),
    OptsForModule = options_from_forms(Forms) ++ Opts,
    Errors = typechecker:type_check_forms(Forms, OptsForModule),
    case {ReturnErrors, Errors} of
        {true, _ } ->
            lists:map(fun(Error) -> {File, Error} end, Errors);
        {false, []} ->
            ok;
        {false, [_|_]} ->
            Opts1 = add_source_file_and_forms_to_opts(File, Forms, Opts),
            gradualizer_fmt:print_errors(Errors, Opts1),
            nok
    end.

add_source_file_and_forms_to_opts(File, Forms, Opts) ->
    Opts1 = [{filename, File}, {forms, Forms} | Opts],
    case filename:extension(File) == ".erl" andalso filelib:is_file(File) of
        true -> [{source_file, File} | Opts1];
        false -> Opts1
    end.

%% Extract -gradualizer(Options) from AST
-spec options_from_forms(gradualizer_file_utils:abstract_forms()) -> options().
options_from_forms([{attribute, _L, gradualizer, Opts} | Fs]) when is_list(Opts) ->
    Opts ++ options_from_forms(Fs);
options_from_forms([{attribute, _L, gradualizer, Opt} | Fs]) ->
    [Opt | options_from_forms(Fs)];
options_from_forms([_F | Fs]) -> options_from_forms(Fs);
options_from_forms([]) -> [].
