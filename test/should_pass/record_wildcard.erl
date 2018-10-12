-module(record_wildcard).

-export([f/0, g/0]).

-record(rec, {apa  = 1         :: integer()
	     ,bepa = false     :: boolean()
	     ,cepa = undefined :: atom()
	     }).

f() ->
    #rec{ apa = 1
	, _   = true }.

-spec g() -> #rec{}.
g() ->
    #rec{ apa = 1
	, _   = true }.
