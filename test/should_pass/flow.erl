-module(flow).

-export([foo/0, bar/1]).

foo() ->
	bar(apa).

-spec bar(apa | bepa) -> true | false.
bar(apa) ->
	true;
bar(bepa) ->
	false.
