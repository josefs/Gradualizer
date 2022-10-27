-module(lists).

%% This module contains specs to replace incorrect or inexact specs in OTP. The
%% commented-out specs are the original specs given in OTP 21 (stdlib-3.7.1)

-type deep_list(A) :: [A | deep_list(A)].

%% flatten/1,2 are defined using recursive constraints, which is something we
%% don't handle yet. We use a recursive user-defined type deep_list/1 instead.

%% -spec flatten(DeepList) -> List when
%%       DeepList :: [term() | DeepList],
%%       List :: [term()].
-spec flatten(deep_list(A))      -> [A].

%% -spec flatten(DeepList, Tail) -> List when
%%       DeepList :: [term() | DeepList],
%%       Tail :: [term()],
%%       List :: [term()].
-spec flatten(deep_list(A), [A]) -> [A].

%% The original fold{l,r} and mapfold{l,r} functions don't require that Acc is
%% the same type throughout the procedure. We do.

%% -spec foldl(Fun, Acc0, List) -> Acc1 when
%%       Fun :: fun((Elem :: T, AccIn) -> AccOut),
%%       Acc0 :: term(),
%%       Acc1 :: term(),
%%       AccIn :: term(),
%%       AccOut :: term(),
%%       List :: [T],
%%       T :: term().
-spec foldl(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
-spec foldr(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.

%% Preserve the (non)empty property of the input list.
-spec map(fun((A) -> B), [A, ...]) -> [B, ...];
         (fun((A) -> B), [A]) -> [B].

%% -spec mapfoldl(Fun, Acc0, List1) -> {List2, Acc1} when
%%       Fun :: fun((A, AccIn) -> {B, AccOut}),
%%       Acc0 :: term(),
%%       Acc1 :: term(),
%%       AccIn :: term(),
%%       AccOut :: term(),
%%       List1 :: [A],
%%       List2 :: [B],
%%       A :: term(),
%%       B :: term().
-spec mapfoldl(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
-spec mapfoldr(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
