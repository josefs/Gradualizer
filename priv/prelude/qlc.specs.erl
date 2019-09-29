-module(qlc).

%% This module contains specs to replace incorrect or inexact specs in OTP.

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
