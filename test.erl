-module(test).

-compile([export_all]).

tokenize(File) ->
    {ok, Binary} = file:read_file(File),
    case erl_scan:string(binary_to_list(Binary)) of
	{error, ErrorInfo, ErrorLoc} ->
	    io:format("Error: ~p~n~p~n",[ErrorInfo,ErrorLoc]);
	{ok, Tokens, EndLoc} ->
	    case erl_parse:parse_form(Tokens) of
		{error, ErrorInfoP} ->
		    io:format("Parse error: ~p~n", [ErrorInfoP]);
		{ok, AbsForm} ->
		    AbsForm
	    end
    end.
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
