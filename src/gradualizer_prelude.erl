-module(gradualizer_prelude).

%% This module contains specs to replace incorrect or inexact specs in OTP.

-spec erlang:apply(function(), [any()]) -> any().
-spec erlang:apply(module(), atom(), [any()]) -> any().
-spec erlang:binary_to_term(binary()) -> any().
-spec erlang:binary_to_term(binary(), [safe | used]) -> any() | {any(), pos_integer()}.
-spec erlang:element(pos_integer(), tuple()) -> any().
-spec erlang:erase() -> [{any(), any()}].
-spec erlang:erase(any()) -> any() | undefined.
-spec erlang:fun_info(function()) -> [{Item, any()}] when
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
-spec erlang:fun_info(function(), Item) -> {Item, any()} when
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
-spec erlang:get() -> [{any(), any()}].
-spec erlang:get(any()) -> any() | undefined.
-spec erlang:get_keys() -> [any()].
-spec erlang:get_stacktrace() -> [{ module()
				  , atom()
				  , arity() | [any()]
				  , [{file, string()}|{line, pos_integer()}]}].
-spec erlang:hd([A, ...]) -> A.
-spec erlang:max(A, B) -> A | B.
-spec erlang:min(A, B) -> A | B.
-spec erlang:port_call(port() | atom(), integer(), any()) -> any().
%% TODO: erlang:process_info
-spec erlang:put(any(), any()) -> any().
-spec erlang:raise(Class, Reason, Stacktrace) -> no_return() when
      Class      :: error | exit | throw,
      Reason     :: any(),
      Stacktrace :: [{module(), atom(), arity() | [any()]} |
                     {function(), [any()]}] |
                    [{module(), atom(), arity() | [any()], [{atom(), any()}]} |
                     {function(), [any()], [{atom(), any()}]}].
-spec erlang:send(pid() | port() | atom() | {atom(), node()}, any()) -> any().
-spec erlang:send(pid() | port() | atom() | {atom(), node()}, any()
                 ,[nosuspend | noconnect])
                -> ok | nosuspend | noconnect.
%% TODO: erlang:system_info({allocator, atom()) -> [term()]
%% TODO: erlang:system_info({allocator_sizes, atom()) -> [term()]
%% TODO: erlang:system_info(os_monotonic_time_source) -> [{atom(), term()}].
%% TODO: erlang:system_info(os_system_time_source) -> [{atom(), term()}].
%% TODO: erlang:system_info(Item :: c_compiler_used) -> {atom(), term()}.
%% TODO: erlang:system_info(Item :: check_io) -> [term()]
-spec erlang:tl([A, ...]) -> [A].
-spec erlang:tuple_to_list(tuple()) -> list().



%% change return Val to any() from term()
-spec application:get_env(Par) -> 'undefined' | {'ok', Val} when
      Par :: atom(),
      Val :: any().
-spec application:get_env(Application, Par) -> 'undefined' | {'ok', Val} when
      Application :: atom(),
      Par :: atom(),
      Val :: any().
-spec application:get_env(Application, Par, Def) -> Val when
      Application :: atom(),
      Par :: atom(),
      Def :: term(),
      Val :: any().

-spec lists:append([[T]])     -> [T].
-spec lists:append([T], [T])  -> [T].
-spec lists:delete(T, [T])    -> [T].
-spec lists:droplast([T, ...]) -> [T].

-spec lists:dropwhile(fun((T) -> boolean()), [T]) -> [T].

-spec lists:duplicate(non_neg_integer(), T) -> [T].

-spec lists:filter(fun((T) -> boolean()), [T]) -> [T].
-spec lists:filtermap(fun((Elem) -> boolean() | {'true', Value}), [Elem])
		     -> [Elem | Value].

-spec lists:flatmap(fun((A) -> [B]), [A]) -> [B].

-type deep_list(A) :: [A | deep_list(A)].

-spec lists:flatten(deep_list(A))      -> [A].
-spec lists:flatten(deep_list(A), [A]) -> [A].

-spec lists:foldl(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
-spec lists:foldr(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
-spec lists:join(T, [T]) -> [T].

-spec lists:foreach(fun((T) -> term()), [T]) -> ok.

-spec lists:last([T, ...]) -> T.

-spec lists:map(fun((A) -> B), [A]) -> [B].
-spec lists:mapfoldl(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
-spec lists:mapfoldr(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.

-spec lists:max([T, ...]) -> T.
-spec lists:min([T, ...]) -> T.

-spec lists:merge([[T]]) -> [T].
-spec lists:merge([X], [Y]) -> [X | Y].
-spec lists:merge(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
-spec lists:merge3([X], [Y], [Z]) -> [X | Y | Z].

-spec lists:nth    (pos_integer(), [T, ...]) -> T.
-spec lists:nthtail(pos_integer(), [T, ...]) -> [T].

-spec lists:partition(fun((T) -> boolean()), [T]) -> {[T], [T]}.

-spec lists:reverse([T]) -> [T].

-spec lists:reverse([T], [T]) -> [T].

-spec lists:sort([T]) -> [T].
-spec lists:sort(fun((T, T) -> boolean()), [T]) -> [T].

-spec lists:split(non_neg_integer(), [T]) -> {[T], [T]}.

-spec lists:splitwith(fun((T) -> boolean()), [T]) -> {[T], [T]}.

-spec lists:sublist([T],                non_neg_integer()) -> [T].
-spec lists:sublist([T], pos_integer(), non_neg_integer()) -> [T].

-spec lists:subtract([T], [T]) -> [T].

-spec lists:takewhile(fun((T) -> boolean()), [T]) -> [T].

-spec lists:umerge([[T]]) -> [T].
-spec lists:umerge([X], [Y]) -> [X | Y].
-spec lists:umerge(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
-spec lists:umerge3([X], [Y], [Z]) -> [X | Y | Z].

-spec lists:unzip ([{A, B}])    -> {[A], [B]}.
-spec lists:unzip3([{A, B, C}]) -> {[A], [B], [C]}.

-spec lists:usort([T]) -> [T].
-spec lists:usort(fun((T, T) -> boolean()), [T]) -> [T].

-spec lists:zip ([A], [B])      -> [{A, B}].
-spec lists:zip3([A], [B], [C]) -> [{A, B, C}].
-spec lists:zipwith (fun((X, Y)    -> T), [X], [Y])      -> [T].
-spec lists:zipwith3(fun((X, Y, Z) -> T), [X], [Y], [Z]) -> [T].

%% Proplists: Changing term() to any() in the most frequently used functions.
-spec proplists:delete(any(), list()) -> list().
-spec proplists:get_all_values(any(), list()) -> list().
-spec proplists:get_value(any(), list()) -> any().
-spec proplists:get_value(any(), list(), any()) -> any().
-spec proplists:get_bool(any(), list()) -> boolean().
-spec proplists:get_keys(list()) -> list().
-spec proplists:lookup(any(), list()) -> none | tuple().
-spec proplists:lookup_all(any(), list()) -> [tuple()].
-spec proplists:append_values(any(), list()) -> list().
-spec proplists:substitute_aliases([{any(), any()}], list()) -> list().

%% qlc

-type cache() :: ets | list | no.
-type key_pos() :: pos_integer() | [pos_integer()].
-type max_list_size() :: non_neg_integer().
-type tmp_file_usage() :: allowed | not_allowed | info_msg
                        | warning_msg  | error_msg.
-type tmp_directory() :: [] | file:name().
-type query_handle_or_list() :: qlc:query_handle() | list().

-spec qlc:e(query_handle_or_list()) ->
		   [any()] | {error, module(), file_sorter:reason()}.
-spec qlc:e(query_handle_or_list(), [Option] | Option) ->
		   [any()] | {error, module(), file_sorter:reason()} when
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}
              | {unique_all, boolean()} | unique_all.
-spec qlc:eval(query_handle_or_list()) ->
		      [any()] | {error, module(), file_sorter:reason()}.

-spec qlc:eval(query_handle_or_list(), Options) ->
		   [any()] | {error, module(), file_sorter:reason()} when
      Options :: [Option] | Option,
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}
              | {unique_all, boolean()} | unique_all.
-spec qlc:fold(fun((any(),any()) -> any()), any(), query_handle_or_list()) ->
		      any() | {error, module(), file_sorter:reason()}.
-spec qlc:fold(fun((any(),any()) -> any()), any(), query_handle_or_list(), Options) ->
		      any() | {error, module(), file_sorter:reason()} when
      Options :: [Option] | Option,
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}.
-spec qlc:next_answers(qlc:query_cursor()) ->
			      [any()] | {error, module(), file_sorter:reason()}.
-spec qlc:next_answers(qlc:query_cursor(), all_remaining | pos_integer()) ->
			      [any()] | {error, module(), file_sorter:reason()}.
-spec qlc:transform_from_evaluator(erl_parse:abstract_expr()
				  ,erl_eval:binding_struct()) ->
					  {ok, erl_parse:abstract_expr()}
					| {not_ok, {error, module(), any()}}.

%% Mnesia

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

-spec mnesia:start() -> 'ok' | {'error', any()}.
-spec mnesia:start([{Option::atom(), Value::_}]) -> 'ok' | {'error', any()}.
-spec mnesia:stop() -> 'stopped' | {'error', any()}.
-spec mnesia:change_config(Config::config_key(), Value::config_value()) ->
				  config_result().
-spec mnesia:transaction(Fun) -> t_result(Res) when
      Fun :: fun(() -> Res).
-spec mnesia:transaction(Fun, Retries) -> t_result(Res) when
      Fun :: fun(() -> Res),
      Retries :: non_neg_integer() | 'infinity';
			(Fun, [Arg::_]) -> t_result(Res) when
      Fun :: fun((...) -> Res).
-spec mnedia:transaction(Fun, [Arg::_], Retries) -> t_result(Res) when
      Fun :: fun((...) -> Res),
      Retries :: non_neg_integer() | 'infinity'.
-spec mnesia:sync_transaction(Fun) -> t_result(Res) when
      Fun :: fun(() -> Res).
-spec mnesia:sync_transaction(Fun, Retries) -> t_result(Res) when
      Fun :: fun(() -> Res),
      Retries :: non_neg_integer() | 'infinity';
                      (Fun, [Arg::_]) -> t_result(Res) when
      Fun :: fun((...) -> Res).
-spec mnesia:sync_transaction(Fun, [Arg::_], Retries) -> t_result(Res) when
      Fun :: fun((...) -> Res),
      Retries :: non_neg_integer() | 'infinity'.
-spec mnesia:activity(Kind, Fun) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun(() -> Res).
-spec mnesia:activity(Kind, Fun, [Arg::_]) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun((...) -> Res);
              (Kind, Fun, Mod) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun(() -> Res),
      Mod  :: atom().
-spec mnesia:activity(Kind, Fun, [Arg::_], Mod) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun((...) -> Res),
      Mod  :: atom().
-spec mnedia:first(atom()) -> any().
-spec mnesia:last(atom()) -> any().
-spec mnesia:next(atom(), any()) -> any().
-spec mnesia:prev(atom(), any()) -> any().
-spec mnesia:select(atom(), ets:match_spec()) -> [any()].
-spec mnesia:select(atom(), ets:match_spec(), lock_kind()) -> [any()].
-spec mnesia:select(any()) -> {[any()], any()} | '$end_of_table'.
-spec mnesai:all_keys(atom()) -> [any()].
-spec mnesia:dirty_select(atom(), ets:match_spec()) -> [any()].
-spec mnesia:dirty_all_keys(atom()) -> [any()].
-spec mnesia:dirty_first(atom()) -> any().
-spec mnesia:dirty_last(atom()) -> any().
-spec mnesia:dirty_next(atom(), any()) -> any().
-spec mnesia:dirty_prev(atom(), any()) -> any().
-spec mnesia:table_info(atom(), any()) -> any().
-spec mnesia:get_backend_types() -> [any()].
-spec mnesia:get_index_plugins() -> [any()].
-spec mnesia:system_info(any()) -> any().
-spec mnesia:create_schema([node()]) -> 'ok' | {'error', any()}.
-spec mnesia:create_schema([node()], [Prop]) -> 'ok' | {'error', any()} when
      Prop :: BackendType | IndexPlugin,
      BackendType :: {backend_types, [{Name::atom(), Module::module()}]},
      IndexPlugin :: {index_plugins, [{{Name::atom()}, Module::module(), Function::atom()}]}.
-spec mneisa:delete_schema([node()]) -> 'ok' | {'error', any()}.
-spec mneisa:add_backend_type(atom(), module()) -> t_result('ok').
-spec mnesia:backup(any()) -> 'ok' | {'error', any()}.
-spec mnesia:backup(any(), module()) -> 'ok' | {'error', any()}.
-spec mnesia:traverse_backup(any(), any(), Fun, Acc) ->
                             {'ok', Acc} | {'error', any()} when
      Fun :: fun((Items, Acc) -> {Items,Acc}).
-spec mnesia:traverse_backup(any(), module(),
                      any(), module(),
                      Fun, Acc) ->
                             {'ok', Acc} | {'error', any()} when
      Fun :: fun((Items, Acc) -> {Items,Acc}).
-spec mnesia:install_fallback(any()) -> 'ok' | {'error', any()}.
-spec mnesia:install_fallback(any(), module()|[Opt]) ->
                              'ok' | {'error', any()} when
      Opt :: Module | Scope | Dir,
      Module :: {'module', Mod::module()},
      Scope :: {'scope', 'global' | 'local'},
      Dir :: {'mnesia_dir', Dir::string()}.
-spec mnesia:uninstall_fallback() -> 'ok' | {'error', any()}.
-spec mnesia:uninstall_fallback(Args) -> 'ok' | {'error', any()} when
      Args :: [{'mnesia_dir', Dir::string()}].
-spec mnesia:activate_checkpoint([Arg]) -> {'ok', Name, [node()]} | {'error', any()} when
      Arg :: {'name', Name} | {'max', [atom()]} | {'min', [atom()]} |
             {'allow_remote', boolean()} | {'ram_overrides_dump', boolean()}.
-spec mnesia:deactivate_checkpoint(any()) -> 'ok' | {'error', any()}.
-spec mnesia:backup_checkpoint(any(), any()) -> 'ok' | {'error', any()}.
-spec mnesia:backup_checkpoint(any(), any(), module()) -> 'ok' | {'error', any()}.
-spec mnesia:restore(any(), [Arg]) -> t_result([atom()]) when
      Op  :: 'skip_tables' | 'clear_tables' | 'keep_tables' | 'restore_tables',
      Arg :: {'module', module()} | {Op, [atom()]} | {'default_op', Op}.
-spec mnesia:create_table([Arg]) -> t_result('ok') when
      Arg :: {'name', atom()} | create_option().
-spec mnesia:create_table(atom(), [create_option()]) -> t_result('ok').
-spec mnesia:delete_table(atom()) -> t_result('ok').
-spec mnesia:add_table_copy(atom(), node()
			   ,'ram_copies' | 'disc_copies' | 'disc_only_copies')
			   -> t_result(ok).
-spec mnesia:del_table_copy(atom(), node()) -> t_result(ok).
-spec mnesia:move_table_copy(atom(), node(), node()) -> t_result(ok).
-spec mnesia:add_table_index(atom(), atom() | non_neg_integer()) -> t_result(ok).
-spec mnesia:del_table_index(atom(), atom() | non_neg_integer()) -> t_result(ok).
-spec mnesia:transform_table(atom(), Fun, [atom()]) -> t_result(ok) when
      Fun:: fun((tuple()) -> tuple()) | ignore.
-spec mnesia:transform_table(atom(), Fun, [atom()], atom()) -> t_result(ok) when
      Fun:: fun((tuple()) -> tuple()) | ignore.
-spec mnesia:change_table_copy_type(atom(), node()
				   ,'ram_copies' | 'disc_copies' | 'disc_only_copies') -> t_result(ok).
-spec mnesia:clear_table(atom()) -> t_result(ok).
-spec mnesia:read_table_property(atom(), any()) -> tuple().
-spec mnesia:write_table_property(atom(), tuple()) -> t_result(ok).
-spec mnesia:delete_table_property(atom(), any()) -> t_result(ok).
-spec mnesia:change_table_frag(atom(), any()) -> t_result(ok).
-spec mnesia:dump_tables([atom()]) -> t_result(ok).
-spec mnesia:wait_for_tables([atom()], timeout()) ->
      'ok' | {'timeout', [atom()]} | {'error', any()}.
-spec mnesia:force_load_table(atom()) -> 'yes' | {'error', any()}.
-spec mnesia:change_table_access_mode(atom(), Mode) -> t_result(ok) when
      Mode :: 'read_only'|'read_write'.
-spec mnesia:change_table_load_order(atom(), Order) -> t_result(ok) when
      Order :: non_neg_integer().
-spec mnesia:change_table_majority(atom(), boolean()) -> t_result(ok).
-spec mnesia:set_master_nodes([node()]) -> 'ok' | {'error', any()}.
-spec mnesia:set_master_nodes(atom(), [node()]) ->
                              'ok' | {'error', any()}.
-spec mnesia:sync_log() -> 'ok' | {'error', any()}.
-spec mnesia:subscribe(What) -> {'ok', node()} | {'error', any()} when
      What :: 'system' | 'activity' | {'table', atom(), 'simple' | 'detailed'}.
-spec mnesia:unsubscribe(What) -> {'ok', node()} | {'error', any()} when
      What :: 'system' | 'activity' | {'table', atom(), 'simple' | 'detailed'}.
-spec mnesia:load_textfile(file:filename()) -> t_result(ok) | {'error', any()}.
-spec mnesia:dump_to_textfile(file:filename()) -> 'ok' | 'error' | {'error', any()}.
