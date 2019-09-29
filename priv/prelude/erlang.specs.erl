-module(erlang).

%% This module contains specs to replace incorrect or inexact specs in OTP.

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
