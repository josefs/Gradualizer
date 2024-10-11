%% @doc
%% Gradualizer is a <em>static type checker</em> for Erlang
%% with support for <em>gradual typing</em>.
%%
%% A static type checker detects errors thanks to type analysis of a given program.
%% Gradualizer can infer some types, but mostly relies on function specs.
%% In a nutshell, we could say that it checks for consistency of a function body with its
%% declared spec and for consistency of a callee's spec with passed in arguments.
%% Specifically, Gradualizer does not perform entire program flow analysis.
%%
%% Gradual typing means that full type information is not required to raise warnings - any type
%% information is better than no type information. It allows for a _gradual_ transition between
%% fully static typing, which offers the best correctness guarantee, and fully dynamic typing,
%% which means the least overhead of writing specs, the full Erlang expressiveness,
%% and _letting it crash_ at runtime.
%% We can choose the right balance between the static/dynamic extremes depending on the application,
%% project maturity, team size, the need for reusability or for documentation consistency.
%%
%% <strong>
%% If you are interested in using Gradualizer to type check your code,
%% then you might be looking for the Rebar3 plugin {@link rebar_prv_gradualizer}
%% or the command-line interface {@link gradualizer_cli}.
%% </strong>
%%
%% This module contains entry points for calling into Gradualizer as a library,
%% as well as some Erlang shell utilities for working directly with the type checker internals.
%%
%% Let's try out the shell utilities by running:
%%
%% ```
%% $ rebar3 shell
%% '''
%%
%% Now, we can play with some examples:
%%
%% ```
%% > gradualizer:type_of("[ a || _ <- lists:seq(1, 5) ]").
%% {type,0,list,[{atom,0,a}]}
%% > typelib:pp_type(v(-1)).
%% "[a]"
%% > typechecker:normalize(gradualizer:type("a()"), gradualizer:env("-type a() :: integer().", [])).
%% {type,0,integer,[]}
%% > typelib:pp_type(v(-1)).
%% "integer()"
%% > gradualizer:type_of("fun (A) -> #{tag => my_map, list_of_as => [ A || _ <- lists:seq(1, 5) ]} end").
%% {type,0,'fun',
%%       [{type,0,product,[{type,0,any,[]}]},
%%        {type,0,map,
%%              [{type,0,map_field_assoc,[{atom,0,tag},{atom,0,my_map}]},
%%               {type,0,map_field_assoc,
%%                     [{atom,0,list_of_as},{type,0,list,[{type,0,any,[]}]}]}]}]}
%% > typelib:pp_type(v(-1)).
%% "fun((any()) -> #{tag => my_map, list_of_as => [any()]})"
%% '''
%%
%% The main library API of Gradualizer are `type_check_(file|module|dir)' functions,
%% which accept options described by the {@link options()} type.
%% @end
-module(gradualizer).

-export([type_check_file/1, type_check_file/2,
         type_check_module/1, type_check_module/2,
         type_check_dir/1, type_check_dir/2,
         type_check_files/1, type_check_files/2,
         type_check_forms/2]).

-export([type/1,
         env/0, env/1, env/2,
         type_of/1, type_of/2]).

-export_type([options/0, top/0]).

-type options() :: proplists:proplist().
%% The options accepted by the Gradualizer API.
%%
%% <ul>
%%   <li>
%%   `{i, Dir}':
%%   include path for `-include' and `-include_lib' when checking
%%   Erlang source files. Specify multiple times for multiple include paths.
%%   </li>
%%
%%   <li>
%%   `stop_on_first_error':
%%   if `true' stop type checking on the first error,
%%   if `false' continue checking all functions in the given file and all files
%%   in the given directory.
%%   </li>
%%
%%   <li>
%%   `crash_on_error':
%%   if `true' crash on the first produced error.
%%   </li>
%%
%%   <li>
%%   `return_errors':
%%   return a list of errors instead of printing them and returning a single condensed `ok | nok'.
%%   </li>
%%
%%   <li>
%%   `fmt_location':
%%   how to format location when pretty printing errors:
%%     <ul>
%%       <li>
%%       `none':
%%       no location for easier comparison
%%       </li>
%%
%%       <li>
%%       `brief':
%%       for machine processing (`<line>:<column>:' before message text)
%%       </li>
%%
%%       <li>
%%       `verbose' (default): for human readers
%%       (`on line <line> at column <column>' within the message text)
%%       </li>
%%     </ul>
%%   </li>
%%
%%   <li>
%%   `fmt_expr_fun':
%%   function to pretty print an expression AST (useful to support other languages)
%%   </li>
%%
%%   <li>
%%   `fmt_type_fun':
%%   function to pretty print a type AST (useful to support other languages)
%%   </li>
%%
%%   <li>
%%   `{color, always | never | auto}':
%%   Use colors when printing fancy messages.
%%   Auto is the default but auto-detection of tty doesn't work when running
%%   as an escript. It works when running from the Erlang shell though.
%%   </li>
%%
%%   <li>
%%   `{fancy, boolean()}':
%%   Use fancy error messages when possible. True by
%%   default. Doesn't work when a custom `fmt_expr_fun' is used.
%%   </li>
%% </ul>

%% This type is the top of the subtyping lattice. It's never expanded.
%% The definition can be anything apart from any(),
%% so that we don't run into the "opaque type underspecified and therefore meaningless" warning.
-opaque top() :: none().

-include("gradualizer.hrl").
-include("typechecker.hrl").

%% API functions

%% @doc Type check a source or beam file
-spec type_check_file(file:filename()) -> ok | nok | [{file:filename(), any()}].
type_check_file(File) ->
    type_check_file(File, []).

%% @doc Type check a source or beam file
-spec type_check_file(file:filename(), options()) -> ok | nok | [{file:filename(), any()}].
type_check_file(File, Opts) ->
    ExcludeModules = proplists:get_value(exclude_modules, Opts, []),
    AtomBaseFile = list_to_atom(filename:rootname(filename:basename(File))),
    case lists:member(AtomBaseFile, ExcludeModules) of
        true ->
            ok;
        false ->
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
                            ok = gradualizer_db:import_beam_files([File]),
                            type_check_forms(File, Forms, Opts);
                        Error ->
                            throw(Error)
                    end;
                Ext ->
                    throw({unknown_file_extension, Ext})
            end
    end.

%% @doc Runs an erl_lint pass, to check if the forms can be compiled at all,
%% before running the type checker.
-spec lint_and_check_forms(Forms, file:filename(), options()) -> R when
      Forms :: gradualizer_file_utils:abstract_forms(),
      R :: ok | nok | [{file:filename(), any()}].
lint_and_check_forms(Forms, File, Opts) ->
    case erl_lint:module(Forms, File, [return_errors]) of
        {ok, _Warnings} ->
            % import the currently checked file so that it can
            % reference itself even without adding its path via --pa
            ok = gradualizer_db:import_erl_files([File]),

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
            Pattern = filename:join(Dir, "*.{erl,beam}"),
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
                      NewErrors = type_check_file_or_dir(File, Opts),
                      % we can assert because we pass in the return_errors option
                      NewErrors = ?assert_type(NewErrors, [any()]),
                      NewErrors ++ Errors;
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

%% Extract `-gradualizer(Options)' from AST
-spec options_from_forms(gradualizer_file_utils:abstract_forms()) -> options().
options_from_forms([{attribute, _L, gradualizer, Opts} | Fs]) when is_list(Opts) ->
    Opts ++ options_from_forms(Fs);
options_from_forms([{attribute, _L, gradualizer, Opt} | Fs]) ->
    [Opt | options_from_forms(Fs)];
options_from_forms([_F | Fs]) -> options_from_forms(Fs);
options_from_forms([]) -> [].

%% @doc Return a Gradualizer type for the passed in Erlang type definition.
%%
%% ```
%% > gradualizer:type("[integer()]").
%% {type,0,list,[{type,0,integer,[]}]}
%% '''
-spec type(string()) -> typechecker:type().
type(Type) ->
    typelib:remove_pos(typelib:parse_type(Type)).

%% @see env/2
-spec env() -> typechecker:env().
env() ->
    env([]).

%% @see env/2
-spec env(gradualizer:options()) -> typechecker:env().
env(Opts) ->
    env("", Opts).

%% @doc Create a type checker environment populated by types defined in a source code snippet
%% via `-type ...' and `-record(...)' attributes.
%%
%% Currently, it's not possible to define variable bindings in the environment.
%%
%% ```
%% > rr(typechecker).
%% > gradualizer:env("-type a() :: integer().", []).
%% #env{fenv = #{},imported = #{},venv = #{},
%%      tenv = #{module => undefined,records => #{},
%%               types => #{{a,0} => {[],{type,0,integer,[]}}}},
%%      infer = false,verbose = false,exhaust = true,
%%      clauses_stack = [],union_size_limit = 30,
%%      current_spec = none}
%% > gradualizer:env("-record(r, {f}).", []).
%% #env{
%%     fenv = #{},imported = #{},venv = #{},
%%     tenv =
%%         #{module => undefined,
%%           records =>
%%               #{r =>
%%                     [{typed_record_field,
%%                          {record_field,0,{atom,0,f},{atom,1,undefined}},
%%                          {type,0,any,[]}}]},
%%           types => #{}},
%%     infer = false,verbose = false,exhaust = true,
%%     clauses_stack = [],union_size_limit = 30,
%%     current_spec = none}
%% '''
-spec env(string(), gradualizer:options()) -> typechecker:env().
env(ErlSource, Opts) ->
    Forms = gradualizer_lib:ensure_form_list(merl:quote(lists:flatten(ErlSource))),
    ErlParseForms = lists:map(fun revert/1, Forms),
    ParseData = typechecker:collect_specs_types_opaques_and_functions(ErlParseForms),
    typechecker:create_env(ParseData, Opts).

-spec revert(erl_syntax:syntaxTree()) -> erl_parse:abstract_form().
revert(Form) ->
    erl_syntax:is_form(Form) orelse erlang:error({invalid_form, Form}),
    ?assert_type(erl_syntax:revert(Form), erl_parse:abstract_form()).

%% @see type_of/2
-spec type_of(string()) -> typechecker:type().
type_of(Expr) ->
    type_of(Expr, env()).

%% @doc Infer type of an Erlang expression.
%%
%% ```
%% > gradualizer:type_of("[ a || _ <- lists:seq(1, 5) ]").
%% {type,0,list,[{atom,0,a}]}
%% > gradualizer:type_of("case 5 of 1 -> one; _ -> more end").
%% {type,0,union,[{atom,0,more},{atom,0,one}]}
%% > Env = gradualizer:env([{union_size_limit, 1}]).
%% > gradualizer:type_of("case 5 of 1 -> one; _ -> more end", Env).
%% {type,0,any,[]}
%% '''
-spec type_of(string(), typechecker:env()) -> typechecker:type().
type_of(Expr, Env) ->
    [Form] = gradualizer_lib:ensure_form_list(merl:quote(lists:flatten(Expr))),
    {Ty, _Env} = typechecker:type_check_expr(Env, Form),
    Ty.
