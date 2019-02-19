-module(qlc_test).

-export([test/0]).

% This example is from the documentation of qlc

test() ->
    L = [1,2,3],
    Bs = erl_eval:add_binding('L', L, erl_eval:new_bindings()),
    QHE = qlc:string_to_handle("[X+1 || X <- L].", [], Bs),
    case QHE of
	{error, Module, Reason} ->
	    io:format("Error in ~p: ~p~n",[Module, Reason]);
	QH -> qlc:eval(QH)
    end.
