-module(case_pattern_should_fail).
-export([f/2]).

%% Expected error: The variable X is expected to have type atom() but it has type integer().
%% It doesn't throw any error because type_check_expr doesn't check patterns in case expressions
%% as type_check_expr_in does.
-spec f(integer(), atom()) -> ok.
f(X, Y) ->
    case Y of
        X -> anything
    end,
    ok.