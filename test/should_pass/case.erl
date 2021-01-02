-module('case').

-compile([export_all, nowarn_export_all]).

cse(X) ->
    case X of
        true ->
            false;
        false ->
            true
    end.
