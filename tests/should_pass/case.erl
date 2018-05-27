-module('case').

-compile([export_all]).

cse(X) ->
    case X of
	true ->
	    false;
	false ->
	    true
    end.
