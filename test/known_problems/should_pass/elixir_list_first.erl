-module(elixir_list_first).

-export([f/0]).

% This is how List.first/1 is defined in Elixir
-spec first([]) -> nil;
           ([Elem, ...]) -> Elem.
first([]) -> nil;
first([Element | _]) -> Element.

-spec f() -> ok.
f() ->
    first(some_list()),
    ok.

-spec some_list() -> list().
some_list() -> [anything].
