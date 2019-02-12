-module(gradualizer_prelude).

%% This module contains specs to replace incorrect or inexact specs in OTP.

-type ext_binary() :: binary().

-spec erlang:apply(function(), [any()]) -> any().
-spec erlang:apply(atom(), function(), [any()]) -> any().
-spec erlang:binary_to_term(ext_binary()) -> any().
-spec erlang:binary_to_term(ext_binary(), [safe | unsafe]) -> any() | {any(), pos_integer()}.
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
-spec erlang:max(any(), any()) -> any().
-spec erlang:min(any(), any()) -> any().
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
		 , [nosuspend | noconnect])
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
