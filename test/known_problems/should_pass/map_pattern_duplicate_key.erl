-module(map_pattern_duplicate_key).

-export([f/1]).

% Fails with: The expression x on line 6 at column 13 is not a valid key in the map type #{x := a}
-spec f(#{x := a}) -> true.
f(#{x := a, x := a}) -> true.