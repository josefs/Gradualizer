-module(block_scope).

-export([foo/0]).

foo() ->
    begin
	X = 1,
	Y = 2
    end,
    X,Y.
