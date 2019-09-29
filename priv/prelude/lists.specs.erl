-module(lists).

%% This module contains specs to replace incorrect or inexact specs in OTP.

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
