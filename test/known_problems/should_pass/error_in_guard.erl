-module(error_in_guard).
-compile([debug_info]).
-export([f/1]).

-record(s, {a :: c | d}).
-record(r, {a :: #s{} | boolean()}).

-spec f(#r{}) -> boolean().
f(R) -> if
    (R#r.a)#s.a == c -> true;
    true -> false
   end.
