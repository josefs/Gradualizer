%% @doc Collect exported functions and types from multiple files.
%%
%% For exported functions with missing spec, a spec is generated with any()
%% as the type for all parameters and return values.
-module(gradualizer_db).

%% API functions
-export([start_link/0,
         get_spec/3,
         get_type/3, get_exported_type/3, get_opaque_type/3,
         get_record_type/2,
         get_modules/0, get_types/1,
         save/1, load/1,
         import_files/1, import_app/1, import_otp/0]).

%% Callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Types for the Erlang Abstract Format
-type type() :: erl_parse:abstract_type().

%% Gen server local registered name
-define(name, ?MODULE).

%% Internal data
-record(typeinfo, {exported :: boolean(),
                   opaque   :: boolean(),
                   params   :: [{var, erl_anno:anno(), atom()}],
                   body     :: type()}).

%% Public API functions

start_link() ->
    gen_server:start_link({local, ?name}, ?MODULE, #{}, []).

%% @doc Fetches the types of the clauses of an exported function. User-defined
%%      types and record types are annotated with filename on the form
%%      "module.erl"
-spec get_spec(M :: module(),
               F :: atom(),
               A :: arity()) -> {ok, [type()]} | not_found.
get_spec(M, F, A) ->
    call({get_spec, M, F, A}).

%% @doc Fetches an exported or unexported user-defined type. Does not expand
%%      opaque types.
-spec get_type(Module :: module(),
               Type :: atom(),
               Params :: [type()]) -> {ok, type()} | opaque | not_found.
get_type(M, T, A) ->
    call({get_type, M, T, A}).

%% @doc Fetches an exported type. Does not expand opaque types.
-spec get_exported_type(Module :: module(),
                        Type :: atom(),
                        Params :: [type()]) -> {ok, type()} | opaque |
                                               not_exported | not_found.
get_exported_type(M, T, A) ->
    call({get_exported_type, M, T, A}).

%% @doc Like get_type/3 but also expands opaque types.
-spec get_opaque_type(Module :: module(),
                      Type :: atom(),
                      Params :: [type()]) -> {ok, type()} | not_found.
get_opaque_type(M, T, A) ->
    call({get_opaque_type, M, T, A}).

%% @doc Fetches a record type defined in the module.
-spec get_record_type(Module :: module(),
                      Name :: atom()) -> {ok, [type()]} | not_found.
get_record_type(Module, Name) ->
    call({get_record_type, Module, Name}).

%% @doc Return a list of all known modules.
-spec get_modules() -> [module()].
get_modules() ->
    call(get_modules).

-spec get_types(module()) -> [{atom(), arity()}].
get_types(Module) ->
    call({get_types, Module}).

-spec save(Filename :: any()) -> ok | {error, any()}.
save(Filename) ->
    call({save, Filename}).

-spec load(Filename :: any()) -> ok | {error, any()}.
load(Filename) ->
    call({load, Filename}).

import_files(Files) ->
    call({import_files, Files}, infinity).

-spec import_app(App :: atom()) -> ok.
import_app(App) ->
    call({import_app, App}, infinity).

-spec import_otp() -> ok.
import_otp() ->
    call(import_otp, infinity).

%% ----------------------------------------------------------------------------

%% Gen_server

-type opts() :: #{autoimport => boolean()}.
-define(default_opts, #{autoimport => true}).

-record(state, {specs   = #{} :: #{mfa() => [type()]},
                types   = #{} :: #{mfa() => #typeinfo{}},
                records = #{} :: #{{module(), atom()} => [typechecker:typed_record_field()]},
                opts    = ?default_opts :: opts(),
                srcmap  = #{} :: #{module() => string()},
                beammap = #{} :: #{module() => string()},
                loaded  = #{} :: #{module() => boolean()}}).

-type state() :: #state{}.

-spec init(opts()) -> {ok, state()}.
init(Opts0) ->
    Opts = maps:merge(?default_opts, Opts0),
    State1 = #state{opts = Opts},
    State2 = case Opts of
                 #{autoimport := true} ->
                    State1#state{srcmap = get_src_map(), beammap = get_beam_map()};
                 _ ->
                    State1
             end,
    {ok, State2}.

-spec handle_call(any(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({get_spec, M, F, A}, _From, State) ->
    State1 = autoimport(M, State),
    K = {M, F, A},
    case State1#state.specs of
        #{K := Types} ->
            Types1 = [typelib:annotate_user_types(M, Type) || Type <- Types],
            {reply, {ok, Types1}, State1};
        _NoMatch ->
            {reply, not_found, State1}
    end;
handle_call({get_exported_type, M, T, Args}, _From, State) ->
    State1 = autoimport(M, State),
    handle_get_type(M, T, Args, true, false, State1);
handle_call({get_type, M, T, Args}, _From, State) ->
    State1 = autoimport(M, State),
    handle_get_type(M, T, Args, false, false, State1);
handle_call({get_opaque_type, M, T, Args}, _From, State) ->
    State1 = autoimport(M, State),
    handle_get_type(M, T, Args, false, true, State1);
handle_call({get_record_type, M, Name}, _From, State) ->
    State1 = autoimport(M, State),
    K = {M, Name},
    case State1#state.records of
        #{K := TypedFields1} ->
            TypedFields2 =
                [{typed_record_field, Field, typelib:annotate_user_types(M, Type)}
                     || {typed_record_field, Field, Type} <- TypedFields1],
            {reply, {ok, TypedFields2}, State1};
        _ ->
            {reply, not_found, State1}
    end;
handle_call(get_modules, _From, State) ->
    {reply, maps:keys(State#state.srcmap), State};
handle_call({get_types, M}, _From, State) ->
    State1 = autoimport(M, State),
    Ts = [{T, A} || {Mod, T, A} <- maps:keys(State#state.types), Mod == M],
    {reply, Ts, State1};
handle_call({save, Filename}, _From, State) ->
    Permanent = {State#state.specs, State#state.types, State#state.loaded},
    Bin = term_to_binary(Permanent, [compressed]),
    Res = file:write_file(Filename, Bin),
    {reply, Res, State};
handle_call({load, Filename}, _From, State) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            try
                {Sp2, Ty2, Loaded2} = binary_to_term(Bin),
                #state{specs = Sp1, types = Ty1, loaded = Loaded1} = State,
                NewState = State#state{specs  = maps:merge(Sp1, Sp2),
                                       types  = maps:merge(Ty1, Ty2),
                                       loaded = maps:merge(Loaded1, Loaded2)},
                {reply, ok, NewState}
            catch error:E ->
                {reply, {error, E, erlang:get_stacktrace()}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({import_module, Mod}, _From, State) ->
    case import_module(Mod, State) of
        {ok, State1} ->
            {reply, ok, State1};
        not_found ->
            {reply, not_found, State}
    end;
handle_call({import_files, Files}, _From, State) ->
    State1 = import_files(Files, State),
    {reply, ok, State1};
handle_call({import_app, App}, _From, State) ->
    Pattern = code:lib_dir(App) ++ "/src/*.erl",
    Files = filelib:wildcard(Pattern),
    State1 = import_files(Files, State),
    {reply, ok, State1};
handle_call(import_otp, _From, State) ->
    Pattern = code:lib_dir() ++ "/*/src/*.erl",
    Files = filelib:wildcard(Pattern),
    State1 = import_files(Files, State),
    {reply, ok, State1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %when Reason == normal; Reason == shutdown
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------------

%% Helpers

%% @doc ensure DB server is started
call(Request) ->
    call(Request, 5000).

call(Request, Timeout) ->
    try gen_server:call(?name, Request, Timeout)
    catch exit:{noproc, _} ->
            {ok, _} = start_link(),
            gen_server:call(?name, Request, Timeout)
    end.

%% helper for handle_call for get_type, get_exported_type, get_opaque_type.
-spec handle_get_type(module(), Name :: atom(), Params :: [type()],
                      RequireExported :: boolean(), ExpandOpaque :: boolean(),
                      state()) -> {reply, {ok, type()} | atom(), state()}.
handle_get_type(M, T, Args, RequireExported, ExpandOpaque, State) ->
    K = {M, T, length(Args)},
    case State#state.types of
        #{K := TypeInfo} ->
            case TypeInfo of
                 #typeinfo{exported = false} when RequireExported ->
                     {reply, not_exported, State};
                 #typeinfo{opaque = true} when not ExpandOpaque ->
                     {reply, opaque, State};
                 #typeinfo{params = Vars,
                           body = Type0} ->
                     VarMap = maps:from_list(lists:zip(Vars, Args)),
                     Type1 = typelib:substitute_type_vars(Type0, VarMap),
                     Type2 = typelib:annotate_user_types(M, Type1),
                     {reply, {ok, Type2}, State}
             end;
        _NoMatch ->
            {reply, not_found, State}
    end.

-spec autoimport(module(), state()) -> state().
autoimport(_M, #state{opts = #{autoimport := false}} = State) ->
    State;
autoimport(M, #state{opts = #{autoimport := true},
                     loaded = Loaded} = State) ->
    case Loaded of
        #{M := _} ->
            %% Alrady loaded or attempted
            State;
        _ ->
            %io:format("Loading types from ~p~n", [M]),
            case import_module(M, State) of
                {ok, State1} -> State1;
                not_found    -> State
            end
    end.

import_module(Mod, State) ->
    case State#state.beammap of
        #{Mod := Filename} ->
            State1 = import_files([Filename], State),
            {ok, State1};
        _ ->
          case State#state.srcmap of
              #{Mod := Filename} ->
                  State1 = import_files([Filename], State),
                  {ok, State1};
              _ ->
                  not_found
          end
    end.

import_files([File | Files], State) ->
    {ok, Forms} =
        case re:run(File, beam_file_regexp()) of
            {match, _} ->
                {ok, gradualizer:get_forms_from_beam(File)};
            nomatch ->
                EppOpts = [{includes, guess_include_dirs(File)}],
                epp:parse_file(File, EppOpts)
        end,
    [{attribute, _, file, _},
     {attribute, _, module, Module} | Forms1] = Forms,
    check_epp_errors(File, Forms1),
    Specs    = collect_specs(Module, Forms1),
    SpecMap1 = add_entries_to_map(Specs, State#state.specs),
    Types    = collect_types(Module, Forms1),
    Records  = collect_records(Module, Forms1),
    TypeMap1 = add_entries_to_map(Types, State#state.types),
    RecMap1  = add_entries_to_map(Records, State#state.records),
    Loaded1  = (State#state.loaded)#{Module => true},
    State1   = State#state{specs   = SpecMap1,
                           types   = TypeMap1,
                           records = RecMap1,
                           loaded  = Loaded1},
    import_files(Files, State1);
import_files([], St) ->
    St.

%% Include dirs for OTP apps are given in makefiles. We can never
%% guarrantee to get them right without extracting the types during
%% compilation.
guess_include_dirs(File) ->
    Dir = filename:dirname(File),
    case filename:basename(Dir) of
        "src" -> [Dir ++ "/../include"];
        _     -> []
    end ++ [code:lib_dir(M) ++ "/include" || M <- [erts, kernel, stdlib]].

%% Log warnings for epp errors among the given forms
%% Bad errors are failed includes due to bad include paths.
-spec check_epp_errors(file:filename(), Forms :: [tuple()]) -> ok.
check_epp_errors(File, Forms) ->
    Errors          = [E || {error, E} <- Forms],
    MissingIncludes = [F || {_Line, epp, {include, file, F}} <- Errors],
    if
        MissingIncludes /= [] ->
            error_logger:warning_msg("Failed to find the following include"
                                     " files for ~p:~n~p",
                                     [File, MissingIncludes]);
        Errors /= [] ->
            error_logger:warning_msg("Errors while loading ~p:~n~p",
                                     [File, Errors]);
        true ->
            ok
    end.

-spec add_entries_to_map([{Key, Value}], #{K => V}) -> #{K => V}
                                             when Key :: K, Value :: V.
add_entries_to_map(Entries, Map) ->
    lists:foldl(fun ({MFA, Types}, MapAcc) ->
                    %% Maybe TODO: Warn if an element is overwritten
                    MapAcc#{MFA => Types}
                end,
                Map,
                Entries).

-spec collect_types(module(), Forms :: [tuple()]) -> [{mfa(), #typeinfo{}}].
%% Collect exported types, including opaques, record definitions,
%% exported and unexported types
collect_types(Module, Forms) ->
    %% ExportedTypes :: [{atom(), arity()}]
    ExportedTypes = lists:concat([Tys || {attribute, _, export_type,
                                          Tys} <- Forms]),

    %% Now all type definitions are easy to extract.
    Types = [begin
                 Id       = {Module, Name, length(Vars)},
                 Exported = lists:member({Name, length(Vars)}, ExportedTypes),
                 Params   = [VarName || {var, _, VarName} <- Vars],
                 Info     = #typeinfo{exported = Exported,
                                      opaque   = (Attr == opaque),
                                      params   = Params,
                                      body     = Body},
                 {Id, Info}
             end || {attribute, _, Attr, {Name, Body, Vars}} <- Forms,
                    Attr == type orelse Attr == opaque,
                    is_atom(Name)],
    Types.

collect_records(Module, Forms) ->
    [{{Module, Name}, Fields} || {Name, Fields} <- extract_record_defs(Forms)].



%% Normalize Type Defs
%% -------------------
%%
%% Extracts and normalizes type definitions from a list of forms.
%%
%% Normalise record definitions into types (i.e. typed record definitions).
%% That is, if there is no typed definition of a record among the
%% forms, create one from the untyped one and normalize so that they
%% all have a default value.
%%
-spec extract_record_defs(Forms :: [tuple()]) -> Typedefs :: [{atom(), [type()]}].
extract_record_defs([{attribute, L, record, {Name, _UntypedFields}},
                     {attribute, L, type, {{record, Name}, Fields, []}} |
                     Rest]) ->
    %% This representation is only used in OTP < 19
    TypedFields = lists:map(fun absform:normalize_record_field/1, Fields),
    R = {Name, TypedFields},
    [R | extract_record_defs(Rest)];
extract_record_defs([{attribute, _L, record, {Name, Fields}} | Rest]) ->
    %% Convert type typed record
    TypedFields = lists:map(fun absform:normalize_record_field/1, Fields),
    R = {Name, TypedFields},
    [R | extract_record_defs(Rest)];
extract_record_defs([_ | Rest]) ->
    %% Skip forms that are not record definitions
    extract_record_defs(Rest);
extract_record_defs([]) ->
    [].

%% Returns specs for all exported functions, generating any-types for unspeced
%% functions.
-spec collect_specs(module(), [tuple()]) -> [{mfa(), [type()]}].
collect_specs(Module, Forms) ->
    Specs = [normalize_spec(Spec, Module) ||
                 {attribute, _, spec, Spec} <- Forms],
    ExportAll = lists:any(fun ({attribute, _, compile, CompileOpts})
                                when is_list(CompileOpts) ->
                                  lists:member(export_all, CompileOpts);
                              ({attribute, _, compile, export_all}) ->
                                  true;
                              (_) ->
                                  false
                          end,
                          Forms),
    Exports =
        if ExportAll ->
               [{Name, Arity} || {function, _, Name, Arity, _} <- Forms];
           true ->
               lists:concat([Exs || {attribute, _, export, Exs} <- Forms])
        end,

    SpecedFunsSet = sets:from_list([{F, A} || {{M, F, A}, _} <- Specs,
                                              M == Module]),
    ImplicitSpecs = [make_spec(Module, F, A) ||
            {F, A} <- Exports,
            not sets:is_element({F, A},
                        SpecedFunsSet)],
    [{Key, absform:normalize_function_type_list(Types)}
     || {Key, Types} <- Specs ++ ImplicitSpecs].

normalize_spec({{Func, Arity}, Types}, Module) ->
    {{Module, Func, Arity}, Types};
normalize_spec(Spec = {{M, F, A}, _Types}, Module) ->
    M /= Module andalso error_logger:info_report([{spec_for, {M,F,A}},
                                                  {found_in, Module}]),
    Spec.

-spec make_spec(module(), atom(), arity()) -> {mfa(), [type()]}.
make_spec(Module, Name, Arity) ->
    {{Module, Name, Arity}, [make_function_type(Arity)]}.

%% Creates the function type (any(), any(), ...) -> any().
make_function_type(Arity) ->
    A = erl_anno:new(0),
    {type, A, 'fun',
     [{type, A, product, lists:duplicate(Arity, {type, A, any, []})},
      {type, A, any, []}]}.

-spec get_src_map() -> #{module() => file:filename()}.
get_src_map() ->
    SrcDirs = [case lists:reverse(Path) of
                   "nibe/" ++ Tail -> lists:reverse("lre.*/crs/" ++ Tail);
                   RevPath         -> lists:reverse("lre.*/" ++ RevPath)
               end || Path <- code:get_path()],
    SrcFiles = lists:flatmap(fun filelib:wildcard/1, SrcDirs),
    Pairs = [begin
                 {match, [Mod]} = re:run(Filename, erl_file_regexp(),
                                         [{capture, all_but_first, list}]),
                 {list_to_atom(Mod), Filename}
             end || Filename <- SrcFiles],
    maps:from_list(Pairs).

-spec get_beam_map() -> #{module() => file:filename()}.
get_beam_map() ->
    BeamDirs = code:get_path(),
    BeamFiles = lists:flatmap(fun (Dir) -> filelib:wildcard(Dir ++ "/*.beam") end, BeamDirs),
    BeamPairs = lists:map(fun (Filename) ->
            {match, [Mod]} = re:run(Filename, beam_file_regexp(), [{capture, all_but_first, list}]),
            {list_to_atom(Mod), Filename}
        end,
        BeamFiles),
    maps:from_list(BeamPairs).

-spec beam_file_regexp() -> {re_pattern, _, _, _}.
beam_file_regexp() ->
    {ok, RE} = re:compile(<<"^.+\/([^/]+)\.beam$">>),
    RE.

-spec erl_file_regexp() -> {re_pattern, _, _, _}.
erl_file_regexp() ->
    {ok, RE} = re:compile(<<"([^/.]*)\.erl$">>),
    RE.
