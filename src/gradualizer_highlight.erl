-module(gradualizer_highlight).

-export([prettyprint_and_highlight/3,
         highlight_in_source/3]).

-include("gradualizer.hrl").

-define(context_before, 2).
-define(context_after,  0).
-define(marker_char,    $^).
-define(color_text,     "\e[36m"). % 36=cyan
-define(color_marker,   "\e[36m").
-define(color_end,      "\e[0m").
%% 31 = Red, 91 = Bright red, 4 = Underline, 1 = Bold

%% erl_parse:erl_parse_tree() is documented but not exported :-(
-type erl_parse_tree() :: erl_parse:abstract_clause()
                        | erl_parse:abstract_expr()
                        | erl_parse:abstract_form()
                        | erl_parse:abstract_type().

%% Pretty-prints and highlights a node in an AST. `AstNode' must be a node
%% existing in the list `AstContext` (forms).
%%
%% To highlight a node in an AST without having the original source code:
%% Pretty-print the AST and parse again. Then, find the corresponding node
%% in the new AST. Find its location. Find its length by pretty-printing,
%% tokenizing and then checking the location and length of the last token.
%% Then print it all in a fancy way with the node highlighted.
-spec prettyprint_and_highlight(AstNode    :: erl_parse_tree(),
                                AstContext :: [erl_parse_tree()],
                                Color      :: boolean()) -> iolist().
prettyprint_and_highlight(AstNode, AstContext, Color) ->
    TrimmedCtx = trim_context(AstNode, AstContext),
    {Pretty, NewNode, _NewCtx} =
        case recreate_source(AstNode, TrimmedCtx) of
            not_found when TrimmedCtx /= AstContext ->
                %% Trimming may have discarded the containing form.
                %% This can happen if line numbers in the AST are
                %% not in order. Try without the full context.
                recreate_source(AstNode, AstContext);
            {_,_,_} = Found ->
                Found
        end,
    highlight_in_source(NewNode, Pretty, Color).

%% Recreates the source code of abstract forms and a node within it by
%% pretty-printing it. The node is localized in the pretty-printed
%% text by parsing the pretty-printed source code and matching them
%% side by side to find the corresponding AstNode in the pretty-printed
%% source code.
recreate_source(AstNode, AstCtx) ->
    Pretty = prettyprint_forms(AstCtx),
    NewCtx = parse_into_list_of_forms(Pretty),
    case find_in_asts(AstNode, AstCtx, NewCtx) of
        {found, NewNode} ->
            {Pretty, NewNode, NewCtx};
        not_found ->
            %% Could be that the line numbers in the original AST are not
            %% in ascending order.
            not_found
    end.

%% Highlights a node in the AST in a piece of source code. The node must
%% be annotated with line and column information for this to work. Returns
%% an inlist().
highlight_in_source(AstNode, Source, Color) ->
    {Start, End} = find_start_and_end_location_in_source(AstNode, Source),
    highlight_text(Source, Start, End, Color).

%% Finds the start and end location of an abstract node in source code.
%% This is done by pretty-printing, tokenizing, counting the tokens and
%% then finding the corresponding tokens in the tokenized source code.
-spec find_start_and_end_location_in_source(Node :: erl_parse_tree(), Source :: string()) ->
                                   {Start :: erl_anno:location(), End :: erl_anno:location()}.
find_start_and_end_location_in_source(Node, Source) ->
    StartLoc = min_location_in_ast(Node),
    %% Get the location after the last token of Node
    %% TODO: Generalize to not only Erlang (e.g. configurable fun)
    {ok, Tokens, _} = erl_scan:string(Source, {1,1}, [text]),
    TokensStartingAtNode =
        lists:dropwhile(fun (T) ->
                                erl_anno:location(element(2,T)) < StartLoc
                        end, Tokens),
    NumTokens = num_tokens(Node),
    [LastToken] = lists:sublist(TokensStartingAtNode, NumTokens, 1),
    EndLoc = {_,_} = erl_scan:end_location(LastToken),
    {StartLoc, EndLoc}.

min_location_in_ast(AstNode) ->
    erl_parse:fold_anno(fun (Anno, Min) ->
                                Loc = erl_anno:location(Anno),
                                min(Min, Loc)
                        end,
                        erl_anno:location(element(2, AstNode)),
                        AstNode).

%% Returns the number of tokens in an AST node.
%% This is done by pretty-printing it and tokensizing it again.
-spec num_tokens(Ast :: erl_parse_tree()) -> pos_integer().
num_tokens(Ast) ->
    Pretty = erl_prettypr:format(Ast),
    {ok, Tokens, _End} = erl_scan:string(Pretty, {1,1}),
    ?assert_type(length(Tokens), pos_integer()).

%% Find the form containing the searched node and sometimes a node before
%% as extra context.
trim_context(Node, Forms) ->
    case erl_anno:line(element(2, Node)) of
        0 -> Forms; % generated code or something...
        Line ->
            Forms1 = lists:reverse(Forms),
            %% The containing form and the preceding forms reversed
            FormAndPreforms =
                lists:dropwhile(fun (Form) ->
                                        L = erl_anno:line(element(2, Form)),
                                        L > Line
                                end, Forms1),
            case FormAndPreforms of
                [] ->
                    %% Line numbers seem to be wrong
                    Forms;
                [FoundForm|PreForms1] ->
                    %% Include any preceding forms starting within 2 lines
                    %% above our searched form
                    PreForms2 =
                        lists:takewhile(fun (Form) ->
                                                L = erl_anno:line(element(2, Form)),
                                                L >= Line - 2
                                        end, PreForms1),
                    lists:reverse([FoundForm|PreForms2])
            end
    end.

-spec prettyprint_forms(Forms :: [erl_parse_tree()]) -> string().
prettyprint_forms(Forms) ->
    lists:flatten([[erl_prettypr:format(Form), "\n"] || Form <- Forms]).

-spec parse_into_list_of_forms(Forms :: string()) -> [erl_parse_tree()].
parse_into_list_of_forms(Pretty) ->
    case merl:quote({1,1}, Pretty) of
        Forms when is_list(Forms) -> Forms;
        Form                      -> [Form]
    end.

%% Highlights the text between two locations (line and column) in a text.
highlight_text(Pretty, StartLoc, EndLoc, Color) ->
    Lines = re:split(Pretty, "\\n", [{return, list}]),
    HiLines = lists:flatmap(fun ({Line, LineNo}) ->
                                    highlight_line(Line, LineNo, StartLoc, EndLoc, Color)
                            end,
                            lists:zip(Lines, lists:seq(1, length(Lines)))),
    %% trim leading empty lines
    HiLines1 = lists:dropwhile(fun ("") -> true; (_) -> false end, HiLines),
    [[Line, "\n"] || Line <- HiLines1].

%% Highlights a line between two locations by returning a list of lines:
%% If the line is highlighted, one highliged line and a marker line is
%% returned. If a line is too far from the highlighted area, no lines are
%% returned. Otherwise, a list containing the unchanged line is returned.
-spec highlight_line(Line :: string(), LineNo :: erl_anno:line(),
                     Start :: erl_anno:location(), End :: erl_anno:location(),
                     Color :: boolean()) -> [string()].
highlight_line(_Line, N, {L1, _}, {L2, _}, _Color) when N < L1 - ?context_before;
                                                        N > L2 + ?context_after ->
    []; % Not included
highlight_line(Line, N, {L1, _}, {L2, _}, _Color) when N < L1; N > L2 ->
    [Line]; % Not highlighted context
highlight_line(Line, N, {N, C1}, {N, C2}, Color) ->
    %% Only this line highlighted, from C1 to C2
    color_and_mark_line(Line, C1, C2, Color);
highlight_line(Line, N, {L1, C1}, {L2, _C2}, Color) when L1 == N, N < L2 ->
    %% First line
    color_and_mark_line(Line, C1, ?assert_type(length(Line) + 1, pos_integer()), Color);
highlight_line(Line, N, {L1, _C1}, {L2, C2}, Color) when L1 < N, N == L2 ->
    %% Last line
    color_and_mark_line(Line, step_spaces(Line, 1), C2, Color);
highlight_line(Line, N, {L1, _}, {L2, __}, Color) when L1 < N, N < L2 ->
    %% Internal line
    color_and_mark_line(Line, step_spaces(Line, 1), length(Line) + 1, Color).

-spec step_spaces(string(), erl_anno:column()) -> erl_anno:column().
step_spaces([$\t | Str], Col) -> step_spaces(Str, Col + 1);
step_spaces([$\s | Str], Col) -> step_spaces(Str, Col + 1);
step_spaces(_Str,        Col) -> Col.

blank([$\t | Str]) -> [$\t | blank(Str)];
blank([_Ch | Str]) -> [$\s | blank(Str)];
blank("")          -> "".

%% Color the string from column C1 to (not including) C2. Returns
%% a list containing the colored line and a line with ^^^^ markers.
-spec color_and_mark_line(Line :: string(), StartCol :: erl_anno:column(),
                          EndCol :: erl_anno:column(), Color :: boolean()) -> [string()].
color_and_mark_line(Line, C1, C2, Color) ->
    {Pre, Rest} = lists:split(C1 - 1, Line),
    {Mid, Post} = lists:split(C2 - C1, Rest),
    {ColorText, ColorMarker, ColorEnd} =
        case Color of
            true  -> {?color_text, ?color_marker, ?color_end};
            false -> {"", "", ""}
        end,
    [Pre ++ ColorText ++ Mid ++ ColorEnd ++ Post,
     blank(Pre) ++ ColorMarker ++ lists:duplicate(C2 - C1, ?marker_char) ++ ColorEnd].

%% Finds a node in an AST and return its corresponding node in another AST.
%% The AST is searched in depth first order.
-spec find_in_asts(OldNode, OldAst, NewAst) -> {found, NewNode} | not_found
        when OldNode      :: erl_parse_tree(),
             OldAst       :: erl_parse_tree() | [erl_parse_tree()],
             NewAst       :: erl_parse_tree() | [erl_parse_tree()],
             NewNode      :: erl_parse_tree().
find_in_asts(_OldNode, [], []) ->
    not_found;
find_in_asts(OldNode, [X|Xs], [Y|Ys]) ->
	case find_in_asts(OldNode, X, Y) of
		{found, _} = Found -> Found;
	    not_found          -> find_in_asts(OldNode, Xs, Ys)
	end;
find_in_asts(OldNode, OldNode, NewNode) ->
    {found, NewNode}; % Found the node!
find_in_asts(OldNode, OldAst, NewAst) when is_tuple(OldAst),
                                           is_tuple(NewAst) ->
    find_in_asts(OldNode,
                 gradualizer_lib:get_ast_children(OldAst),
                 gradualizer_lib:get_ast_children(NewAst));
find_in_asts(_OldNode, _OldAst, _NewAst) ->
    not_found. % mismatch
