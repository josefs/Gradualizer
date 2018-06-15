-module(ty).

% This module provides helper functions which can act as type annotations for
% when a programmer wants to add more type information into a program.

-compile([export_all]).

% Boolean

-spec boolean() -> boolean().
boolean() ->
    true.

-spec boolean(boolean()) -> boolean().
boolean(B) ->
    B.

% Int

-spec int() -> integer().
int() ->
    1.

-spec int(integer()) -> integer().
int(X) ->
    X.

% Float

-spec float() -> float().
float() ->
    1.0.

-spec float(float()) -> float().
float(X) ->
    X.

% List

-spec list(A) -> [A].
list(A) ->
    [A].

-spec list(A, [A]) -> [A].
list(_,L) ->
    L.

% Bit strings

-spec bit() -> <<>>.
bit() ->
    <<>>.

%%% Typing bitstrings properly would require dependent types.

-spec bit1() -> <<_:1>>.
bit1() ->
    <<1>>.

% Functions

-spec fn1(A,R, any()) -> fun((A) -> R).
fn1(_,_,F) ->
    F.

-spec fn2(A,B,R, any()) -> fun((A,B) -> R).
fn2(_,_,_,F) ->
    F.

-spec fn3(A,B,C,R, any()) -> fun((A,B,C) -> R).
fn3(_,_,_,_,F) ->
    F.

% Tuples

-spec tup1(A, any()) -> {A}.
tup1(_, T) ->
    T.

-spec tup1(A) -> {A}.
tup1(A) ->
    {A}.

-spec tup2(A,B, any()) -> {A,B}.
tup2(_,_, T) ->
    T.

-spec tup2(A,B) -> {A,B}.
tup2(A,B) ->
    {A,B}.

-spec tup3(A,B,C, {A,B,C}) -> {A,B,C}.
tup3(_,_,_,T) ->
    T.

-spec tup3(A,B,C) -> {A,B,C}.
tup3(A,B,C) ->
    {A,B,C}.

-spec tup4(A,B,C,D, {A,B,C,D}) -> {A,B,C,D}.
tup4(_,_,_,_, T) ->
    T.

-spec tup4(A,B,C,D) -> {A,B,C,D}.
tup4(A,B,C,D) ->
    {A,B,C,D}.

-spec tup5(A,B,C,D,E, {A,B,C,D,E}) -> {A,B,C,D,E}.
tup5(_,_,_,_,_, T) ->
    T.

-spec tup5(A,B,C,D,E) -> {A,B,C,D,E}.
tup5(A,B,C,D,E) ->
    {A,B,C,D,E}.

% Union

-spec union(A,B, A | B) -> A | B.
union(_,_, U) ->
    U.

-spec union(A,B) -> A | B.
union(A, _) ->
    A.

% Any

-spec any() -> any().
any() ->
    any.

-spec any(any()) -> any().
any(A) ->
    A.
