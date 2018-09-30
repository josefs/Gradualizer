-module(rebar_prv_gradualizer).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gradualizer).
-define(DEPS, [app_discovery]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar gradualizer"},
            {opts, []},
            {short_desc, "typecheck the project with gradualizer"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    CheckedApps = lists:map(fun gradualizer_check_app/1, rebar_state:project_apps(State)),
    HasNok = lists:member(nok, CheckedApps),
    if
        HasNok -> {error, {?MODULE, undefined}};
        true -> {ok, State}
    end.

-spec gradualizer_check_app(rebar_app_info:t()) -> ok | nok.
gradualizer_check_app(App) ->
    GOpts = rebar_app_info:get(App, gradualizer_opts, []),
    Files = files_to_check(App),
    gradualizer:type_check_files(Files, GOpts).

-spec files_to_check(rebar_app_info:t()) -> [file:name()].
files_to_check(App) ->
    Opts = rebar_app_info:opts(App),
    GOpts = rebar_app_info:get(App, gradualizer_opts, []),
    Include = proplists:get_value(include, GOpts, undefined),
    Exclude = proplists:get_value(exclude, GOpts, []),
    Cwd = rebar_app_info:dir(App),

    Patterns = case Include of
        undefined ->
            {SrcDirs, ExtraDirs} = resolve_src_dirs(Opts),
            lists:map(fun(File) ->
                filename:join(filename:absname(File, Cwd), "*.erl")
            end, SrcDirs ++ ExtraDirs);
        _ -> Include
    end,
    Files = lists:flatmap(fun (Pattern) ->
            filelib:wildcard(filename:absname(Pattern, Cwd))
        end, Patterns),
    ExpandedExclude = lists:flatmap(fun (Pattern) ->
                filelib:wildcard(filename:absname(Pattern, Cwd))
            end, Exclude),
    lists:filter(
        fun (File) ->
            not lists:member(File, ExpandedExclude)
        end, Files).

-spec format_error(any()) -> string().
format_error(_) ->
    "Gradualizer found errors.".

-spec resolve_src_dirs(dict:dict()) -> {[file:name()], [file:name()]}.
resolve_src_dirs(Opts) ->
    SrcDirs = rebar_dir:src_dirs(Opts, ["src"]),
    ExtraDirs = rebar_dir:extra_src_dirs(Opts, []),
    normalize_src_dirs(SrcDirs, ExtraDirs).

%% remove duplicates and make sure no directories that exist
%% in src_dirs also exist in extra_src_dirs
-spec normalize_src_dirs([file:name()], [file:name()]) -> {[file:name()], [file:name()]}.
normalize_src_dirs(SrcDirs, ExtraDirs) ->
    S = lists:usort(SrcDirs),
    E = lists:subtract(lists:usort(ExtraDirs), S),
    {S, E}.
