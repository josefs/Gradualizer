-module(union_with_any).

%% T | any() means
%% "at least values of type T are possible; maybe also other values"

-export([f1/1, f2/0]).

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

-spec inc(integer()) -> integer().
inc(N) -> N + 1.
