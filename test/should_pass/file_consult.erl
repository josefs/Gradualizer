-module(file_consult).

-export([f/0]).

-spec f() -> [integer()].
f() ->
    {ok, Terms} = file:consult("primes.txt"),
    Terms.
