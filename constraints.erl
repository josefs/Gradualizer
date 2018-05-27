-module(constraints).

-export([empty/0, combine/1, combine/2, convert/1]).

-type type() :: erl_parse:abstract_type().

-record(constraints, {
	  lower_bounds = #{} :: #{ string => type() },
	  upper_bounds = #{} :: #{ string => type() }
	 }).


empty() ->
    #constraints{}.

% add({T1, T2}, Cs) ->

combine(C1, C2) ->
    combine([C1, C2]).

combine([]) ->
    empty();
combine([Cs]) ->
    Cs;
combine([C1, C2 | Cs]) ->
    C = #constraints{ lower_bounds = maps:merge(C1#constraints.lower_bounds
					       ,C2#constraints.lower_bounds)
		    , upper_bounds = maps:merge(C1#constraints.upper_bounds
					       ,C2#constraints.upper_bounds)
		    },
    combine([C | Cs]).

convert(Cs) when is_list(Cs) ->
    combine(lists:map(fun convert/1, Cs));

convert({type, _,constraint,[{atom, _,is_subtype},[TV = {var, _, V}
						  ,TW = {var, _, W}]]}) ->
    #constraints{ upper_bounds = #{ V => TW }
		, lower_bounds = #{ W => TV } };
convert({type, _,constraint,[{atom, _,is_subtype},[{var, _, V},Ty]]}) ->
    #constraints{ upper_bounds = #{ V => Ty } }.
