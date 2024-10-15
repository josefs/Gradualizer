-module(case_pattern).

-export([f1/2, f2/2]).

-spec f1(integer(), atom()) -> ok.
f1(X, Y) ->
    case Y of
        X -> ok
    end.

-spec f2(integer(), atom()) -> ok.
f2(X, Y) ->
    case Y of
        X -> anything
    end,
    ok.
