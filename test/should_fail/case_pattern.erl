-module(case_pattern).
-export([f/2]).

-spec f(integer(), atom()) -> ok.
f(X, Y) ->
    case Y of
        X -> ok
    end.
