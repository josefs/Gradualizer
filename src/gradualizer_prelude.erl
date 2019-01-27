-module(gradualizer_prelude).

%% This module contains specs to replace incorrect or inexact specs in OTP.

-spec erlang:hd([A, ...]) -> A.
-spec erlang:tl([A, ...]) -> [A].


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

-spec lists:duplicate(T, non_neg_integer()) -> [T].

-spec lists:filter(fun((T) -> boolean() | {true, Value}), [T]) -> [T | Value].

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

-spec lists:split(integer(), [T]) -> {[T], [T]}.

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

