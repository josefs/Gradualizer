-module(record_wildcard1).

-export([g/0]).

-record(rec, {apa  = 1         :: integer()
	     ,bepa = false     :: boolean()
	     ,cepa = undefined :: atom()
	     ,depa = 1.0       :: float()
	     }).

-spec g() -> #rec{}.
g() ->
    #rec{ apa = 1
	, _   = true }.
