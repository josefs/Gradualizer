-module(case_pattern2).
-export([f/2]).

-spec f(integer(), atom()) -> ok.
f(X, Y) ->
    case {X, Y} of
        {Z, Z} -> ok
    end.
