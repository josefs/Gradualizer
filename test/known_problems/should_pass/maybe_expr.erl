-module(maybe_expr).
-export([syntax/0]).

syntax() ->
    maybe
        ok ?= ok
    end.
