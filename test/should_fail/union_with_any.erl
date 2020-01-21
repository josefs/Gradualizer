-module(union_with_any).

%% T | any() means
%% "at least values of type T are possible; maybe also other values"

-export([f1/1, f2/0, f3/1]).

%% With spec
-spec f1(atom() | any()) -> any().
f1(X) -> inc(X).

%% Without spec
f2() ->
    AtomOrAny = receive
                    1   -> get_atom();
                    Any -> Any
                end,
    inc(AtomOrAny). %% Fails because atom() is possible

-spec get_atom() -> atom().
get_atom() -> banana.

-spec inc(number()) -> number().
inc(N) -> N + 1.

-record(r, {i :: integer()}).

%% The variable is expected to have type #r{} but it has type undefined | any()
-spec f3(any() | undefined) -> integer().
f3(R) ->
    R#r.i.
