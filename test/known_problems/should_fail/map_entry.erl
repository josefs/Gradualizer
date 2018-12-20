-module(map_entry).
-export([f/0]).

-spec f() -> #{bepa := apa}.
f() -> #{apa => bepa}.
