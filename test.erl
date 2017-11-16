-module(test).

-compile([export_all]).

-spec tokenize(string()) -> atom().
tokenize(File) ->
    {ok, Binary} = file:read_file(File),
    case erl_scan:string(binary_to_list(Binary)) of
	{error, ErrorInfo, ErrorLoc} ->
	    io:format("Error: ~p~n~p~n",[ErrorInfo,ErrorLoc]);
	{ok, Tokens, _EndLoc} ->
	    Tokens
    end.

parse(File) ->
    Tokens = tokenize(File),
    Toks = collect_forms(Tokens,[]),
    Forms = lists:map(fun erl_parse:parse_form/1, Toks).
    %% case file:open(File,[read]) of
    %% 	{error, Reason} ->
    %% 	    io:format("File error: ~p~n",[Reason]);
    %% 	{ok, Handle} ->
    %% 	    file:read(Handle)
    %% end.
    %% case erl_scan:string(Str) of
    %% 	{ok, Tokens, EndLoc} ->
    %% 	    foo;
    %% 	{error, ErrorInfo, ErrorLoc} ->
    %% 	    io:format("Error:  ~p~n~p~n",[ErrorInfo,ErrorLoc])
    %% end.

collect_forms([],[]) -> [];
collect_forms([],Acc) ->
    [lists:reverse(Acc)];
collect_forms([Tok={dot,_}|Tokens],Acc) ->
    [lists:reverse([Tok|Acc])|collect_forms(Tokens,[])];
collect_forms([Tok|Tokens],Acc) ->
    collect_forms(Tokens,[Tok|Acc]).

collect_specs([{attribute,_,spec,Spec}|Forms]) ->
    [Spec|collect_specs(Forms)];
collect_specs([_|Forms]) ->
    collect_specs(Forms);
collect_specs([]) -> [].

parse_specs(File) ->
    collect_specs(lists:map(fun ({ok,Form}) -> Form end, parse(File))).

-type type() :: integer | float | boolean | {singleton, atom()}
	      | {range, integer(), integer() }
	      | {tuple, list(type())}
	      | {list, type()}
	      | {function, list(type()), type()}
	      | {union, list(type())}
	      | {intersection, list(type())}
	      | untyped
	      .

-type schema() :: { list(variable()) % parameters
		  , list(variable()) % forall quantified
		  , type()
		  , list(clause()) }.

-type variable() :: integer(). % This will work for now.

-type clause() :: { type () , type() }.
  % We will interpret clauses as subtyping constraints.
  % One option would be to interpret clauses on the form X :: type as
  % type equality, but that wouldn't be very consistent. We can alway
  % recover equality by adding type :: X as a constraint.
  % Perhaps we can argue for a new form of syntax.

