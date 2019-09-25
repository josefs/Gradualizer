-module(nested_pattern_match).
-compile([debug_info]).
-export([f/1]).

-record(s, {a :: c | d}).
-record(r, {a :: #s{}}).

-spec f(#r{}) -> boolean().
f(R) -> if
    (R#r.a)#s.a == c -> true;
    true -> false
   end.
