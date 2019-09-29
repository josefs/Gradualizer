-module(mnesia).

%% This module contains specs to replace incorrect or inexact specs in OTP.

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
