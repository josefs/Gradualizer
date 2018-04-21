%% Copyright 2018 Viktor Söderqvist
%% See the LICENSE file shipped with this file.

%% @doc Collect exported functions and types from multiple files.
%%
%% For exported functions with missing spec, a spec is generated with any()
%% as the type for all parameters and return values.
-module(gradualizer_db).

%% API functions
-export([start_link/0,
         get_spec/3, get_type/3,
         save/1, load/1,
         import_files/1, import_app/1, import_otp/0]).

%% Callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Types for the Erlang Abstract Format
-type line() :: non_neg_integer().

-type type() :: {type, line(), atom(), [type()] | any} |
                {var, line(), atom()} |
                {integer, line(), integer()} |
                {atom, line(), atom()}.

%% Gen server local registered name
-define(name, ?MODULE).

%% Internal data
-record(typeinfo, {exported :: boolean(),
                   opaque   :: boolean(),
                   params   :: [{var, _, atom()}],
                   body     :: type()}).

%% Public API functions

start_link() ->
    gen_server:start_link({local, ?name}, ?MODULE, #{}, []).

-spec get_spec(M :: module(), F :: atom(), A :: arity()) ->
        {ok, [type()]} | not_found.
get_spec(M, F, A) ->
    gen_server:call(?name, {get_spec, M, F, A}).

-spec get_type(Module :: module(), Type :: atom(), Params :: [type()]) ->
        {ok, type()} | not_found.
get_type(M, T, A) ->
    gen_server:call(?name, {get_type, M, T, A}).

-spec save(Filename :: any()) -> ok | {error, any()}.
save(Filename) ->
    gen_server:call(?name, {save, Filename}).

-spec load(Filename :: any()) -> ok | {error, any()}.
load(Filename) ->
    gen_server:call(?name, {load, Filename}).

import_files(Files) ->
    gen_server:call(?name, {import_files, Files}, infinity).

-spec import_app(App :: atom()) -> ok.
import_app(App) ->
    gen_server:call(?name, {import_app, App}, infinity).

-spec import_otp() -> ok.
import_otp() ->
    gen_server:call(?name, import_otp, infinity).

%% ----------------------------------------------------------------------------

%% Gen_server

-type opts() :: #{autoimport => boolean()}.
-define(default_opts, #{autoimport => true}).

-record(state, {specs  = #{} :: #{mfa() => [type()]},
                types  = #{} :: #{mfa() => #typeinfo{}},
                opts   = ?default_opts :: opts(),
                srcmap = #{} :: #{module() => string()},
                loaded = #{} :: #{module() => boolean()}}).

-type state() :: #state{}.

-spec init(opts()) -> {ok, state()}.
init(Opts0) ->
    Opts = maps:merge(?default_opts, Opts0),
    State1 = #state{opts = Opts},
    State2 = case Opts of
                 #{autoimport := true} ->
                    State1#state{srcmap = get_src_map()};
                 _ ->
                    State1
             end,
    {ok, State2}.

-spec handle_call(any(), pid(), state()) -> {reply, state()}.
handle_call({get_spec, M, F, A}, _From, State) ->
    State1 = autoimport(M, State),
    K = {M, F, A},
    case State1#state.specs of
        #{K := Types} -> {reply, {ok, Types}, State1};
        _NoMatch      -> {reply, not_found, State1}
    end;
handle_call({get_type, M, T, A}, _From, State) ->
    State1 = autoimport(M, State),
    K = {M, T, length(A)},
    case State1#state.types of
        #{K := TypeInfo} ->
            Type1 = case TypeInfo of
                        #typeinfo{opaque = true} ->
                            %% Don't expand opaques (toggle option?)
                            {opaque, M, T, A};
                        #typeinfo{params = Vars,
                                  body = Type} ->
                            %% TODO: Add another function 'get_exported_type'
                            %%       that checks the 'exported' flag
                            VarMap = maps:from_list(lists:zip(Vars, A)),
                            substitute_type_vars(Type, VarMap)
                    end,
            {reply, {ok, Type1}, State1};
        _NoMatch ->
            {reply, not_found, State}
    end;
handle_call({save, Filename}, _From, State) ->
    Str = io_lib:format("~p.~n", [State]),
    Gz  = zlib:gzip(Str),
    Res = file:write_file(Filename, Gz),
    {reply, Res, State};
handle_call({load, Filename}, _From, State) ->
    case file:read_file(Filename) of
        {ok, Gz} ->
            try
                Bin = zlib:gunzip(Gz),
                S = binary_to_list(Bin),
                {ok, Tokens, _} = erl_scan:string(S),
                {ok, {Sp2, Ty2}} = erl_parse:parse_term(Tokens),
                true = is_map(Sp2) and is_map(Ty2),
                #state{specs = Sp1, types = Ty1} = State,
                NewState = State#state{specs = maps:merge(Sp1, Sp2),
                                       types = maps:merge(Ty1, Ty2)},
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
    case State#state.srcmap of
        #{Mod := Filename} ->
            State1 = import_files([Filename], State),
            {ok, State1};
        _ ->
            not_found
    end.

import_files([File | Files], State) ->
    {ok, Forms} = epp:parse_file(File, []),
    [{attribute, _, file, _},
     {attribute, _, module, Module} | Forms1] = Forms,
    Specs    = collect_specs(Module, Forms1),
    SpecMap1 = add_entries_to_map(Specs, State#state.specs),
    Types    = collect_types(Module, Forms1),
    TypeMap1 = add_entries_to_map(Types, State#state.types),
    Loaded1  = (State#state.loaded)#{Module => true},
    State1   = State#state{specs  = SpecMap1,
                           types  = TypeMap1,
                           loaded = Loaded1},
    import_files(Files, State1);
import_files([], St) ->
    St.

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

    %% Records are represented as types with name = {record, RecordName}
    TypeDefs = normalize_type_defs(Forms),

    %% Now all type definitions are easy to extract.
    Types = [begin
                 Id       = {Module, Name, length(Vars)},
                 Exported = lists:member(Id, ExportedTypes),
                 Params   = [VarName || {var, _, VarName} <- Vars],
                 Info     = #typeinfo{exported = Exported,
                                      opaque   = (Attr == opaque),
                                      params   = Params,
                                      body     = Body},
                 {Id, Info}
             end || {attribute, _, Attr, {Name, Body, Vars}} <- TypeDefs,
                    Attr == type orelse Attr == opaque],
    Types.

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
-spec normalize_type_defs(Forms :: [tuple()]) -> Typedefs :: [tuple()].
normalize_type_defs([{attribute, L, record, {Name, _UntypedFields}},
                     {attribute, L, type, {{record, Name}, Fields, []}} = R |
                     Rest]) ->
    TypedFields = lists:map(fun normalize_record_field/1, Fields),
    R = {attribute, L, type, {{record, Name}, TypedFields, []}},
    [R | normalize_type_defs(Rest)];
normalize_type_defs([{attribute, L, record, {Name, UntypedFields}} | Rest]) ->
    %% Convert type typed record
    TypedFields = lists:map(fun normalize_record_field/1, UntypedFields),
    R = {attribute, L, type, {{record, Name}, TypedFields, []}},
    [R | normalize_type_defs(Rest)];
normalize_type_defs([{attribute, _, Attr, {_Name, _Body, _Params}} = T | Rest])
                                        when Attr == type; Attr == opaque ->
    %% Normal type or opaque definition
    [T | normalize_type_defs(Rest)];
normalize_type_defs([_ | Rest]) ->
    %% Skip forms that are not type definitions
    normalize_type_defs(Rest);
normalize_type_defs([]) ->
    [].

%% Turns all records into typed records and all record fields into typed
%% record fields. Adds default 'undefined' if default is missing.
normalize_record_field({record_field, L, Name = {atom, _, _}}) ->
    {typed_record_field,
     {record_field, L, Name, {atom, L, undefined}},
     {type, L, any, []}};
normalize_record_field({record_field, L, Name = {atom, _, _}, Default}) ->
    {typed_record_field,
     {record_field, L, Name, Default},
     {type, L, any, []}};
normalize_record_field({typed_record_field,
                        {record_field, L, Name = {atom, _, _}},
                        Type}) ->
    {typed_record_field,
     {record_field, L, Name, {atom, L, undefined}},
     Type};
normalize_record_field({typed_record_field,
                        {record_field, _L, {atom, _, _Name}, _Default},
                        _Type} = Complete) ->
    Complete.

-spec substitute_type_vars(type(),
                           %[{{atom(), arity()}, typeinfo()}],
                           #{atom() => type()}) -> type().
substitute_type_vars({type, L, T, Params}, TVars) when is_list(Params) ->
    {type, L, T, [substitute_type_vars(P, TVars) || P <- Params]};
substitute_type_vars({var, L, Var}, TVars) ->
    case TVars of
        #{Var := Type} -> Type;
        _              -> {var, L, Var}
    end;
substitute_type_vars(Other, _) ->
    Other.

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
    Specs ++ ImplicitSpecs.

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
    {type, 0, 'fun',
     [{type, 0, product, lists:duplicate(Arity, {type, 0, any, []})},
      {type, 0, any, []}]}.

-spec get_src_map() -> #{module() => file:filename()}.
get_src_map() ->
    SrcDirs = [case lists:reverse(Path) of
                   "nibe/" ++ Tail -> lists:reverse("lre.*/crs/" ++ Tail);
                   RevPath         -> lists:reverse("lre.*/" ++ RevPath)
               end || Path <- code:get_path()],
    SrcFiles = lists:flatmap(fun filelib:wildcard/1, SrcDirs),
    {ok, RE} = re:compile(<<"([^/.]*)\.erl$">>),
    Pairs = [begin
                 {match, [Mod]} = re:run(Filename, RE,
                                         [{capture, all_but_first, list}]),
                 {list_to_atom(Mod), Filename}
             end || Filename <- SrcFiles],
    maps:from_list(Pairs).
