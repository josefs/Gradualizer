-module(exhaustive_expr).

-export([f/1]).

%% Expected error: Nonexhaustive patterns, example values which are not covered: b.
%% It doesn't throw any error because type_check_expr doesn't do any exhaustiveness checking.
-spec f(a | b) -> ok.
f(X) ->
    case X of
        a -> anything
    end,
    ok.
