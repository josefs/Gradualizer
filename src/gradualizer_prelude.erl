-module(gradualizer_prelude).

-compile({parse_transform, gradualizer_prelude_parse_trans}).

%% This module contains specs to replace incorrect or inexact specs in OTP.
-override_module(erlang).
-spec apply(function(), [any()]) -> any().
-spec apply(module(), atom(), [any()]) -> any().
-spec binary_to_term(binary()) -> any().
-spec binary_to_term(binary(), [safe | used]) -> any() | {any(), pos_integer()}.
-spec element(pos_integer(), tuple()) -> any().
-spec erase() -> [{any(), any()}].
-spec erase(any()) -> any() | undefined.
-spec fun_info(function()) -> [{Item, any()}] when
      Item :: arity
	    | env
	    | index
	    | name
	    | module
	    | new_index
	    | new_uniq
	    | pid
	    | type
	    | uniq.
-spec fun_info(function(), Item) -> {Item, any()} when
      Item :: arity
	    | env
	    | index
	    | name
	    | module
	    | new_index
	    | new_uniq
	    | pid
	    | type
	    | uniq.
-spec get() -> [{any(), any()}].
-spec get(any()) -> any() | undefined.
-spec get_keys() -> [any()].
-spec get_stacktrace() -> [{ module()
				  , atom()
				  , arity() | [any()]
				  , [{file, string()}|{line, pos_integer()}]}].
-spec hd([A, ...]) -> A.
-spec max(A, B) -> A | B.
-spec min(A, B) -> A | B.
-spec port_call(port() | atom(), integer(), any()) -> any().
%% TODO: process_info
-spec put(any(), any()) -> any().
-spec raise(Class, Reason, Stacktrace) -> no_return() when
      Class      :: error | exit | throw,
      Reason     :: any(),
      Stacktrace :: [{module(), atom(), arity() | [any()]} |
                     {function(), [any()]}] |
                    [{module(), atom(), arity() | [any()], [{atom(), any()}]} |
                     {function(), [any()], [{atom(), any()}]}].
-spec send(pid() | port() | atom() | {atom(), node()}, any()) -> any().
-spec send(pid() | port() | atom() | {atom(), node()}, any()
                 ,[nosuspend | noconnect])
                -> ok | nosuspend | noconnect.
%% TODO: system_info({allocator, atom()) -> [term()]
%% TODO: system_info({allocator_sizes, atom()) -> [term()]
%% TODO: system_info(os_monotonic_time_source) -> [{atom(), term()}].
%% TODO: system_info(os_system_time_source) -> [{atom(), term()}].
%% TODO: system_info(Item :: c_compiler_used) -> {atom(), term()}.
%% TODO: system_info(Item :: check_io) -> [term()]
-spec tl([A, ...]) -> [A].
-spec tuple_to_list(tuple()) -> list().


%% change return Val to any() from term()
-override_module(application).
-spec get_env(Par) -> 'undefined' | {'ok', Val} when
      Par :: atom(),
      Val :: any().
-spec get_env(Application, Par) -> 'undefined' | {'ok', Val} when
      Application :: atom(),
      Par :: atom(),
      Val :: any().
-spec get_env(Application, Par, Def) -> Val when
      Application :: atom(),
      Par :: atom(),
      Def :: term(),
      Val :: any().

-override_module(lists).
-spec append([[T]])     -> [T].
-spec append([T], [T])  -> [T].
-spec delete(T, [T])    -> [T].
-spec droplast([T, ...]) -> [T].

-spec dropwhile(fun((T) -> boolean()), [T]) -> [T].

-spec duplicate(non_neg_integer(), T) -> [T].

-spec filter(fun((T) -> boolean()), [T]) -> [T].
-spec filtermap(fun((Elem) -> boolean() | {'true', Value}), [Elem])
		     -> [Elem | Value].

-spec flatmap(fun((A) -> [B]), [A]) -> [B].

-type deep_list(A) :: [A | deep_list(A)].

-spec flatten(deep_list(A))      -> [A].
-spec flatten(deep_list(A), [A]) -> [A].

-spec foldl(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
-spec foldr(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
-spec join(T, [T]) -> [T].

-spec foreach(fun((T) -> term()), [T]) -> ok.

-spec last([T, ...]) -> T.

-spec map(fun((A) -> B), [A]) -> [B].
-spec mapfoldl(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
-spec mapfoldr(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.

-spec max([T, ...]) -> T.
-spec min([T, ...]) -> T.

-spec merge([[T]]) -> [T].
-spec merge([X], [Y]) -> [X | Y].
-spec merge(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
-spec merge3([X], [Y], [Z]) -> [X | Y | Z].

-spec nth    (pos_integer(), [T, ...]) -> T.
-spec nthtail(pos_integer(), [T, ...]) -> [T].

-spec partition(fun((T) -> boolean()), [T]) -> {[T], [T]}.

-spec reverse([T]) -> [T].

-spec reverse([T], [T]) -> [T].

-spec sort([T]) -> [T].
-spec sort(fun((T, T) -> boolean()), [T]) -> [T].

-spec split(non_neg_integer(), [T]) -> {[T], [T]}.

-spec splitwith(fun((T) -> boolean()), [T]) -> {[T], [T]}.

-spec sublist([T],                non_neg_integer()) -> [T].
-spec sublist([T], pos_integer(), non_neg_integer()) -> [T].

-spec subtract([T], [T]) -> [T].

-spec takewhile(fun((T) -> boolean()), [T]) -> [T].

-spec umerge([[T]]) -> [T].
-spec umerge([X], [Y]) -> [X | Y].
-spec umerge(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
-spec umerge3([X], [Y], [Z]) -> [X | Y | Z].

-spec unzip ([{A, B}])    -> {[A], [B]}.
-spec unzip3([{A, B, C}]) -> {[A], [B], [C]}.

-spec usort([T]) -> [T].
-spec usort(fun((T, T) -> boolean()), [T]) -> [T].

-spec zip ([A], [B])      -> [{A, B}].
-spec zip3([A], [B], [C]) -> [{A, B, C}].
-spec zipwith (fun((X, Y)    -> T), [X], [Y])      -> [T].
-spec zipwith3(fun((X, Y, Z) -> T), [X], [Y], [Z]) -> [T].

%% Proplists: Changing term() to any() in the most frequently used functions.
-override_module(proplists).
-spec delete(any(), list()) -> list().
-spec get_all_values(any(), list()) -> list().
-spec get_value(any(), list()) -> any().
-spec get_value(any(), list(), any()) -> any().
-spec get_bool(any(), list()) -> boolean().
-spec get_keys(list()) -> list().
-spec lookup(any(), list()) -> none | tuple().
-spec lookup_all(any(), list()) -> [tuple()].
-spec append_values(any(), list()) -> list().
-spec substitute_aliases([{any(), any()}], list()) -> list().

%% qlc
-override_module(qlc).
-type cache() :: ets | list | no.
-type max_list_size() :: non_neg_integer().
-type tmp_file_usage() :: allowed | not_allowed | info_msg
                        | warning_msg  | error_msg.
-type tmp_directory() :: [] | file:name().
-type query_handle_or_list() :: query_handle() | list().

-spec e(query_handle_or_list()) ->
		   [any()] | {error, module(), file_sorter:reason()}.
-spec e(query_handle_or_list(), [Option] | Option) ->
		   [any()] | {error, module(), file_sorter:reason()} when
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}
              | {unique_all, boolean()} | unique_all.
-spec eval(query_handle_or_list()) ->
		      [any()] | {error, module(), file_sorter:reason()}.

-spec eval(query_handle_or_list(), Options) ->
		   [any()] | {error, module(), file_sorter:reason()} when
      Options :: [Option] | Option,
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}
              | {unique_all, boolean()} | unique_all.
-spec fold(fun((any(),any()) -> any()), any(), query_handle_or_list()) ->
		      any() | {error, module(), file_sorter:reason()}.
-spec fold(fun((any(),any()) -> any()), any(), query_handle_or_list(), Options) ->
		      any() | {error, module(), file_sorter:reason()} when
      Options :: [Option] | Option,
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}.
-spec next_answers(query_cursor()) ->
			      [any()] | {error, module(), file_sorter:reason()}.
-spec next_answers(query_cursor(), all_remaining | pos_integer()) ->
			      [any()] | {error, module(), file_sorter:reason()}.
-spec transform_from_evaluator(erl_parse:abstract_expr()
				  ,erl_eval:binding_struct()) ->
					  {ok, erl_parse:abstract_expr()}
					| {not_ok, {error, module(), any()}}.

%% Mnesia
-override_module(mnesia).
-type t_result(Res) :: {'atomic', Res} | {'aborted', Reason::any()}.
-type activity() :: 'ets' | 'async_dirty' | 'sync_dirty' | 'transaction' | 'sync_transaction' |
                    {'transaction', Retries::non_neg_integer()} |
                    {'sync_transaction', Retries::non_neg_integer()}.
-type config_key() :: extra_db_nodes | dc_dump_limit.
-type config_value() :: [node()] | number().
-type config_result() :: {ok, config_value()} | {error, any()}.
-type write_locks() :: 'write' | 'sticky_write'.
-type read_locks() :: 'read'.
-type lock_kind() :: write_locks() | read_locks().
-type create_option() ::
        {'access_mode', 'read_write' | 'read_only'} |
        {'attributes', [atom()]} |
        {'disc_copies', [node()]} |
        {'disc_only_copies', [node]} |
        {'index', [atom() | non_neg_integer()]} |
        {'load_order', non_neg_integer()} |
        {'majority', boolean()} |
        {'ram_copies', [node()]} |
        {'record_name', atom()} |
        {'snmp', any()} |
        {'storage_properties', [{Backend::module(), [BackendProp::_]}]} |
        {'type', 'set' | 'ordered_set' | 'bag'} |
        {'local_content', boolean()} |
        {'user_properties', proplists:proplist()}.

-spec start() -> 'ok' | {'error', any()}.
-spec start([{Option::atom(), Value::_}]) -> 'ok' | {'error', any()}.
-spec stop() -> 'stopped' | {'error', any()}.
-spec change_config(Config::config_key(), Value::config_value()) ->
				  config_result().
-spec transaction(Fun) -> t_result(Res) when
      Fun :: fun(() -> Res).
-spec transaction(Fun, Retries) -> t_result(Res) when
      Fun :: fun(() -> Res),
      Retries :: non_neg_integer() | 'infinity';
			(Fun, [Arg::_]) -> t_result(Res) when
      Fun :: fun((...) -> Res).
-spec transaction(Fun, [Arg::_], Retries) -> t_result(Res) when
      Fun :: fun((...) -> Res),
      Retries :: non_neg_integer() | 'infinity'.
-spec sync_transaction(Fun) -> t_result(Res) when
      Fun :: fun(() -> Res).
-spec sync_transaction(Fun, Retries) -> t_result(Res) when
      Fun :: fun(() -> Res),
      Retries :: non_neg_integer() | 'infinity';
                      (Fun, [Arg::_]) -> t_result(Res) when
      Fun :: fun((...) -> Res).
-spec sync_transaction(Fun, [Arg::_], Retries) -> t_result(Res) when
      Fun :: fun((...) -> Res),
      Retries :: non_neg_integer() | 'infinity'.
-spec activity(Kind, Fun) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun(() -> Res).
-spec activity(Kind, Fun, [Arg::_]) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun((...) -> Res);
              (Kind, Fun, Mod) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun(() -> Res),
      Mod  :: atom().
-spec activity(Kind, Fun, [Arg::_], Mod) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun((...) -> Res),
      Mod  :: atom().
-spec first(atom()) -> any().
-spec last(atom()) -> any().
-spec next(atom(), any()) -> any().
-spec prev(atom(), any()) -> any().
-spec select(atom(), ets:match_spec()) -> [any()].
-spec select(atom(), ets:match_spec(), lock_kind()) -> [any()].
-spec select(any()) -> {[any()], any()} | '$end_of_table'.
-spec all_keys(atom()) -> [any()].
-spec dirty_select(atom(), ets:match_spec()) -> [any()].
-spec dirty_all_keys(atom()) -> [any()].
-spec dirty_first(atom()) -> any().
-spec dirty_last(atom()) -> any().
-spec dirty_next(atom(), any()) -> any().
-spec dirty_prev(atom(), any()) -> any().
-spec table_info(atom(), any()) -> any().
-spec get_backend_types() -> [any()].
-spec get_index_plugins() -> [any()].
-spec system_info(any()) -> any().
-spec create_schema([node()]) -> 'ok' | {'error', any()}.
-spec create_schema([node()], [Prop]) -> 'ok' | {'error', any()} when
      Prop :: BackendType | IndexPlugin,
      BackendType :: {backend_types, [{Name::atom(), Module::module()}]},
      IndexPlugin :: {index_plugins, [{{Name::atom()}, Module::module(), Function::atom()}]}.
-spec delete_schema([node()]) -> 'ok' | {'error', any()}.
-spec add_backend_type(atom(), module()) -> t_result('ok').
-spec backup(any()) -> 'ok' | {'error', any()}.
-spec backup(any(), module()) -> 'ok' | {'error', any()}.
-spec traverse_backup(any(), any(), Fun, Acc) ->
                             {'ok', Acc} | {'error', any()} when
      Fun :: fun((Items, Acc) -> {Items,Acc}).
-spec traverse_backup(any(), module(),
                      any(), module(),
                      Fun, Acc) ->
                             {'ok', Acc} | {'error', any()} when
      Fun :: fun((Items, Acc) -> {Items,Acc}).
-spec install_fallback(any()) -> 'ok' | {'error', any()}.
-spec install_fallback(any(), module()|[Opt]) ->
                              'ok' | {'error', any()} when
      Opt :: Module | Scope | Dir,
      Module :: {'module', Mod::module()},
      Scope :: {'scope', 'global' | 'local'},
      Dir :: {'mnesia_dir', Dir::string()}.
-spec uninstall_fallback() -> 'ok' | {'error', any()}.
-spec uninstall_fallback(Args) -> 'ok' | {'error', any()} when
      Args :: [{'mnesia_dir', Dir::string()}].
-spec activate_checkpoint([Arg]) -> {'ok', Name, [node()]} | {'error', any()} when
      Arg :: {'name', Name} | {'max', [atom()]} | {'min', [atom()]} |
             {'allow_remote', boolean()} | {'ram_overrides_dump', boolean()}.
-spec deactivate_checkpoint(any()) -> 'ok' | {'error', any()}.
-spec backup_checkpoint(any(), any()) -> 'ok' | {'error', any()}.
-spec backup_checkpoint(any(), any(), module()) -> 'ok' | {'error', any()}.
-spec restore(any(), [Arg]) -> t_result([atom()]) when
      Op  :: 'skip_tables' | 'clear_tables' | 'keep_tables' | 'restore_tables',
      Arg :: {'module', module()} | {Op, [atom()]} | {'default_op', Op}.
-spec create_table([Arg]) -> t_result('ok') when
      Arg :: {'name', atom()} | create_option().
-spec create_table(atom(), [create_option()]) -> t_result('ok').
-spec delete_table(atom()) -> t_result('ok').
-spec add_table_copy(atom(), node()
			   ,'ram_copies' | 'disc_copies' | 'disc_only_copies')
			   -> t_result(ok).
-spec del_table_copy(atom(), node()) -> t_result(ok).
-spec move_table_copy(atom(), node(), node()) -> t_result(ok).
-spec add_table_index(atom(), atom() | non_neg_integer()) -> t_result(ok).
-spec del_table_index(atom(), atom() | non_neg_integer()) -> t_result(ok).
-spec transform_table(atom(), Fun, [atom()]) -> t_result(ok) when
      Fun:: fun((tuple()) -> tuple()) | ignore.
-spec transform_table(atom(), Fun, [atom()], atom()) -> t_result(ok) when
      Fun:: fun((tuple()) -> tuple()) | ignore.
-spec change_table_copy_type(atom(), node()
				   ,'ram_copies' | 'disc_copies' | 'disc_only_copies') -> t_result(ok).
-spec clear_table(atom()) -> t_result(ok).
-spec read_table_property(atom(), any()) -> tuple().
-spec write_table_property(atom(), tuple()) -> t_result(ok).
-spec delete_table_property(atom(), any()) -> t_result(ok).
-spec change_table_frag(atom(), any()) -> t_result(ok).
-spec dump_tables([atom()]) -> t_result(ok).
-spec wait_for_tables([atom()], timeout()) ->
      'ok' | {'timeout', [atom()]} | {'error', any()}.
-spec force_load_table(atom()) -> 'yes' | {'error', any()}.
-spec change_table_access_mode(atom(), Mode) -> t_result(ok) when
      Mode :: 'read_only'|'read_write'.
-spec change_table_load_order(atom(), Order) -> t_result(ok) when
      Order :: non_neg_integer().
-spec change_table_majority(atom(), boolean()) -> t_result(ok).
-spec set_master_nodes([node()]) -> 'ok' | {'error', any()}.
-spec set_master_nodes(atom(), [node()]) ->
                              'ok' | {'error', any()}.
-spec sync_log() -> 'ok' | {'error', any()}.
-spec subscribe(What) -> {'ok', node()} | {'error', any()} when
      What :: 'system' | 'activity' | {'table', atom(), 'simple' | 'detailed'}.
-spec unsubscribe(What) -> {'ok', node()} | {'error', any()} when
      What :: 'system' | 'activity' | {'table', atom(), 'simple' | 'detailed'}.
-spec load_textfile(file:filename()) -> t_result(ok) | {'error', any()}.
-spec dump_to_textfile(file:filename()) -> 'ok' | 'error' | {'error', any()}.

%% Done
-override_module(gradualizer_prelude).
