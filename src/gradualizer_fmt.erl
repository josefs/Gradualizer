-module(gradualizer_fmt).
-export([format_location/2, format_type_error/2, print_errors/2, handle_type_error/2]).

-include("typelib.hrl").

-define(FMT_LOCATION_DEFAULT, verbose).
-define(color_type, "\e[35m"). % 35=magenta
-define(color_end, "\e[0m").

-spec format_location(any(), brief | verbose, any()) -> io_lib:chars().
format_location(Expr, FmtType, Opts) ->
    case proplists:get_value(fmt_location, Opts, ?FMT_LOCATION_DEFAULT) of
        FmtType ->
            format_location(Expr, FmtType);
        _ ->
            ""
    end.

-spec format_location(any(), brief | verbose) -> io_lib:chars().
format_location(Anno, Config) when is_list(Anno); is_integer(Anno) ->
    %% compatibility with old error types which contain bare
    %% annotations instead of the whole expression
    format_location({expr, Anno}, Config);
format_location(Anno = {L, C}, Config) when is_integer(L), is_integer(C) ->
    %% compatibility with old error types which contain bare
    %% annotations instead of the whole expression
    format_location({expr, Anno}, Config);

format_location(Expr, brief) ->
    case erl_anno:location(element(2, Expr)) of
        {Line, Col} ->
            io_lib:format("~p:~p: ", [Line, Col]);
        Line when is_integer(Line) ->
            io_lib:format("~p: ", [Line])
    end;
format_location(Expr, verbose) ->
    case erl_anno:location(element(2, Expr)) of
        {Line, Col} ->
            io_lib:format(" on line ~p at column ~p", [Line, Col]);
        Line when is_integer(Line) ->
            io_lib:format(" on line ~p", [Line])
    end.

-spec format_type_error(any(), any()) -> io_lib:chars().
format_type_error({type_error, Expression, ActualType, ExpectedType}, Opts)
  when is_tuple(Expression) ->
    format_expr_type_error(Expression, ActualType, ExpectedType, Opts);
format_type_error({nonexhaustive, Anno, Example}, Opts) ->
    FormattedExample =
        case Example of
            [X | Xs] ->
                lists:reverse(
                    lists:foldl(fun(A, Acc) ->
                        [erl_pp:expr(A), "\n\t" | Acc]
                    end, [erl_pp:expr(X)], Xs)
                );
            X -> erl_pp:expr(X)
        end,
    io_lib:format(
      "~sNonexhaustive patterns~s~s",
      [format_location(Anno, brief, Opts),
       format_location(Anno, verbose, Opts),
       case proplists:get_value(fmt_location, Opts, ?FMT_LOCATION_DEFAULT) of
           brief ->
               io_lib:format(": ~s\n", FormattedExample);
           verbose ->
               io_lib:format("\nExample values which are not covered:~n\t~s~n", [FormattedExample])
       end]);
format_type_error({call_undef, Anno, Func, Arity}, Opts) ->
    io_lib:format(
      "~sCall to undefined function ~p/~p~s~n",
      [format_location(Anno, brief, Opts),
       Func,
       Arity,
       format_location(Anno, verbose, Opts)]);
format_type_error({call_undef, Anno, Module, Func, Arity}, Opts) ->
    io_lib:format(
      "~sCall to undefined function ~p:~p/~p~s~n",
      [format_location(Anno, brief, Opts),
       Module,
       Func,
       Arity,
       format_location(Anno, verbose, Opts)]);
format_type_error({undef, record, Anno, {Module, RecName}}, Opts) ->
    io_lib:format("~sUndefined record ~p:~p~s~n",
		  [format_location(Anno, brief, Opts),
		   Module,
		   RecName,
		   format_location(Anno, verbose, Opts)]);
format_type_error({undef, record, Anno, RecName}, Opts) ->
    io_lib:format("~sUndefined record ~p~s~n",
		  [format_location(Anno, brief, Opts),
		   RecName,
		   format_location(Anno, verbose, Opts)]);
format_type_error({undef, record_field, FieldName}, Opts) ->
    io_lib:format(
      "~sUndefined record field ~s~s~n",
      [format_location(FieldName, brief, Opts),
       pp_expr(FieldName, Opts),
       format_location(FieldName, verbose, Opts)]);
format_type_error({undef, user_type, Anno, {Name, Arity}}, Opts) ->
    io_lib:format(
      "~sUndefined type ~p/~p~s~n",
      [format_location(Anno, brief, Opts),
       Name,
       Arity,
       format_location(Anno, verbose, Opts)]);
format_type_error({undef, Type, Anno, {Module, Name, Arity}}, Opts)
  when Type =:= user_type; Type =:= remote_type ->
    TypeS = case Type of user_type -> "type"; remote_type -> "remote type" end,
    io_lib:format(
      "~sUndefined ~s ~p:~p/~p~s~n",
      [format_location(Anno, brief, Opts),
       TypeS,
       Module,
       Name,
       Arity,
       format_location(Anno, verbose, Opts)]);
format_type_error({not_exported, remote_type, Anno, {Module, Name, Arity}}, Opts) ->
    io_lib:format(
      "~sThe type ~s:~s/~p~s is not exported~n",
      [format_location(Anno, brief, Opts),
       Module,
       Name,
       Arity,
       format_location(Anno, verbose, Opts)]);
format_type_error({illegal_pattern, Pat}, Opts) ->
    io_lib:format("~sIllegal pattern ~s~s~n",
		  [format_location(Pat, brief, Opts),
		   pp_expr(Pat, Opts),
		   format_location(Pat, verbose, Opts)]);
format_type_error({illegal_record_info, Expr}, Opts) ->
    io_lib:format(
      "~sIllegal record info ~s~s~n",
      [format_location(Expr, brief, Opts),
       pp_expr(Expr, Opts),
       format_location(Expr, verbose, Opts)]);
format_type_error({type_error, list, _Anno, Ty1, Ty}, Opts) ->
    io_lib:format(
      "~sThe type ~s cannot be an element of a list of type ~s~n",
      [format_location(_Anno, brief, Opts),
       pp_type(Ty1, Opts),
       pp_type(Ty, Opts)]);
format_type_error({type_error, list, Anno, Ty}, Opts) ->
    io_lib:format(
      "~sThe expression of type ~s~s is not a list type~n",
      [format_location(Anno, brief, Opts),
       pp_type(Ty, Opts),
       format_location(Anno, verbose, Opts)]);
format_type_error({type_error, cons_pat, Anno, Cons, Ty}, Opts) ->
    io_lib:format(
      "~sThe pattern ~s~s does not have type:~n~s~n",
      [format_location(Anno, brief, Opts),
       pp_expr(Cons, Opts),
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({argument_length_mismatch, Anno, LenTy, LenArgs}, Opts) ->
    io_lib:format(
      "~sThe clause~s is expected to have ~p argument(s) "
      "but it has ~p~n ",
      [format_location(Anno, brief, Opts),
       format_location(Anno, verbose, Opts),
       LenTy,
       LenArgs]);
format_type_error({type_error, unreachable_clause, Anno}, Opts) ->
    io_lib:format(
      "~sThe clause~s cannot be reached~n",
      [format_location(Anno, brief, Opts),
       format_location(Anno, verbose, Opts)]);
format_type_error({type_error, call_arity, Anno, Fun, TyArity, CallArity}, Opts) ->
    io_lib:format(
      "~sThe function ~s~s expects ~p argument~s, but is given ~p~n",
      [format_location(Anno, brief, Opts),
       pp_expr(Fun, Opts),
       format_location(Anno, verbose, Opts),
       TyArity,
       ["s" || TyArity /= 1],
       CallArity]);
format_type_error({type_error, call_intersect, Anno, FunTy, Name}, Opts) ->
    io_lib:format(
      "~sThe type of the function ~s, called~s doesn't match "
      "the surrounding calling context.~n"
      "It has the following type~n~s~n",
      [format_location(Anno, brief, Opts),
       pp_expr(Name, Opts),
       format_location(Anno, verbose, Opts),
       pp_intersection_type(FunTy, Opts)]);
format_type_error({type_error, expected_fun_type, Anno, Func, FunTy}, Opts) ->
    Name = pp_expr(Func, Opts),
    io_lib:format(
      "~sExpected function ~s~s to have a function type,~n"
      "but it has the following type:~n~s~n",
      [format_location(Anno, brief, Opts),
       Name,
       format_location(Anno, verbose, Opts),
       pp_type(FunTy, Opts)]);
format_type_error({type_error, no_type_match_intersection, Anno, Func, FunTy}, Opts) ->
    Name = pp_expr(Func, Opts),
    io_lib:format(
      "~sNone of the types of the function ~s~s matches the "
      "call site. Here's the types of the function:~n~s~n",
      [format_location(Anno, brief, Opts),
       Name,
       format_location(Anno, verbose, Opts),
       pp_intersection_type(FunTy, Opts)]);
format_type_error({type_error, relop, RelOp, Anno, Ty1, Ty2}, Opts) ->
    io_lib:format(
      "~sThe operator ~p~s requires arguments of "
      "compatible types.~nHowever, it has arguments "
      "of type ~s and ~s~n",
      [format_location(Anno, brief, Opts),
       RelOp,
       format_location(Anno, verbose, Opts),
       pp_type(Ty1, Opts),
       pp_type(Ty2, Opts)]);
format_type_error({type_error, op_type_too_precise, '/' = Op, Anno, Ty}, Opts) when ?is_int_type(Ty) ->
    io_lib:format(
      "~sThe operator ~p~s is expected to have type "
      "~s which is not a supertype of float()~n",
      [format_location(Anno, brief, Opts),
       Op,
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({type_error, op_type_too_precise, Op, Anno, Ty}, Opts) ->
    io_lib:format(
      "~sThe operator ~p~s is expected to have type "
      "~s which is too precise to be statically checked~n",
      [format_location(Anno, brief, Opts),
       Op,
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({type_error, arith_error, ArithOp, Anno, Ty1, Ty2}, Opts) ->
    io_lib:format(
      "~sThe operator ~p~s is requires numeric arguments, but "
      "has arguments of type ~s and ~s~n",
      [format_location(Anno, brief, Opts),
       ArithOp,
       format_location(Anno, verbose, Opts),
       pp_type(Ty1, Opts),
       pp_type(Ty2, Opts)]);
format_type_error({type_error, int_error, ArithOp, Anno, Ty1, Ty2}, Opts) ->
    io_lib:format(
      "~sThe operator ~p~s is requires integer arguments, but "
      " has arguments of type ~s and ~s~n",
      [format_location(Anno, brief, Opts),
       ArithOp,
       format_location(Anno, verbose, Opts),
       pp_type(Ty1, Opts),
       pp_type(Ty2, Opts)]);
format_type_error({type_error, arith_error, ArithOp, Anno, Ty}, Opts) ->
    io_lib:format(
      "~sThe operator ~p~s is expected to have type "
      "~s which has no numeric subtypes~n",
      [format_location(Anno, brief, Opts),
       ArithOp,
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({type_error, int_error, IntOp, Anno, Ty}, Opts) ->
    io_lib:format(
      "~sThe operator ~p~s is expected to have type "
      "~s which has no integer subtypes~n",
      [format_location(Anno, brief, Opts),
       IntOp,
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({type_error, non_number_argument_to_plus, Anno, Ty}, Opts) ->
    io_lib:format(
      "~sThe plus expression~s has a non-numeric argument "
      "of type:~n~s~n",
      [format_location(Anno, brief, Opts),
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({type_error, non_number_argument_to_minus, Anno, Ty}, Opts) ->
    io_lib:format(
      "~sThe negated expression~s has a non-numeric argument "
      "of type:~n~s~n",
      [format_location(Anno, brief, Opts),
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({type_error, unary_error, Op, Anno, TargetTy, Ty}, Opts) ->
    io_lib:format(
      "~sThe application of unary '~s'~s is expected to have type "
      "~s, which has no shared subtype with ~s~n",
      [format_location(Anno, brief, Opts),
       Op,
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts),
       pp_type(TargetTy, Opts)]);
format_type_error({type_error, rel_error, LogicOp, Anno, Ty1, Ty2}, Opts) ->
    io_lib:format(
      "~sThe operator ~p~s is given two arguments with "
      "non-compatible types:~n~s~n~s~n",
      [format_location(Anno, brief, Opts),
       LogicOp,
       format_location(Anno, verbose, Opts),
       pp_type(Ty1, Opts),
       pp_type(Ty2, Opts)]);
format_type_error({type_error, operator_pattern, Pat, Ty}, Opts) ->
    io_lib:format(
      "~sThe operator pattern ~s~s is expected to have type "
      "~s~n",
      [format_location(Pat, brief, Opts),
       pp_expr(Pat, Opts),
       format_location(Pat, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({type_error, pattern, Anno, Pat, Ty}, Opts) ->
    io_lib:format(
      "~sThe pattern ~s~s doesn't have the type ~s~n",
      [format_location(Anno, brief, Opts),
       pp_expr(Pat, Opts),
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({unknown_variable, Anno, Var}, Opts) ->
    io_lib:format(
      "~sUnknown variable ~p~s.~n",
      [format_location(Anno, brief, Opts),
       Var,
       format_location(Anno, verbose, Opts)]);
format_type_error({type_error, check_clauses}, _Opts) ->
    %% TODO: Improve quality of type error
    io_lib:format("Type error in clauses", []);
format_type_error({type_error, record_pattern, Anno, Record, Ty}, Opts) ->
    io_lib:format(
      "~sThe record patterns for record #~p~s is expected to have"
      " type ~s.~n",
      [format_location(Anno, brief, Opts),
       Record,
       format_location(Anno, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({type_error, badkey, KeyExpr, MapType}, Opts) ->
    %% Compare to the runtime error raised by maps:get(Key, Map) error:{badkey, Key}.
    io_lib:format(
      "~sThe expression ~s~s is not a valid key in the map type ~s~n",
      [format_location(KeyExpr, brief, Opts),
       pp_expr(KeyExpr, Opts),
       format_location(KeyExpr, verbose, Opts),
       pp_type(MapType, Opts)]);
format_type_error({type_error, receive_after, Anno, TyClauses, TyBlock}, Opts) ->
    io_lib:format(
      "~sThe types in the clauses and the after block are incompatible~n"
      "in the receive statement~s.~n"
      "The type of the clauses is : ~s~n"
      "The type of the after block is : ~s~n",
      [format_location(Anno, brief, Opts),
       format_location(Anno, verbose, Opts),
       pp_type(TyClauses, Opts),
       pp_type(TyBlock, Opts)]);
format_type_error({type_error, cyclic_type_vars, _Anno, Ty, Xs}, Opts) ->
    io_lib:format(
      "~sThe type spec ~s has a cyclic dependency in variable~s ~s~n",
      [format_location(_Anno, brief, Opts),
       pp_type(Ty, Opts),
       [ "s" || length(Xs) > 1 ],
       string:join(lists:map(fun atom_to_list/1, lists:sort(Xs)), ", ")]);
format_type_error({type_error, mismatch, Ty, Expr}, Opts) ->
    io_lib:format(
      "~sThe expression ~s~s does not have type ~s~n",
      [format_location(Expr, brief, Opts),
       pp_expr(Expr, Opts),
       format_location(Expr, verbose, Opts),
       pp_type(Ty, Opts)]);
format_type_error({bad_type_annotation, TypeLit}, Opts) ->
    io_lib:format(
      "~sThe type annotation ~p~s is not a valid type~n",
      [format_location(TypeLit, brief, Opts),
       pp_expr(TypeLit, Opts),
       format_location(TypeLit, verbose, Opts)]);
format_type_error(type_error, _) ->
    io_lib:format("TYPE ERROR~n", []).

-spec format_expr_type_error(gradualizer_type:abstract_expr(),
			typelib:extended_type(),
			typelib:extended_type(),
			proplists:proplist()) -> io_lib:chars().
format_expr_type_error(Expression, ActualType, ExpectedType, Opts) ->
    FancyExpr = try_highlight_in_context(Expression, Opts),
    InlineExpr = case FancyExpr of
                     "" -> " " ++ pp_expr(Expression, Opts);
                     _  -> ""
                 end,
    io_lib:format(
      "~sThe ~s~ts~s is expected "
      "to have type ~ts but it has type ~ts~n~ts",
      [format_location(Expression, brief, Opts),
       describe_expr(Expression),
       InlineExpr,
       format_location(Expression, verbose, Opts),
       pp_type(ExpectedType, Opts),
       pp_type(ActualType, Opts),
       FancyExpr]).

%% Returns the expression highlighted in its context, if possible.  Otherwise,
%% the empty string is returned.
try_highlight_in_context(Expression, Opts) ->
    case use_highlight_in_context(Opts) of
        true ->
            try
                highlight_in_context(Expression, Opts)
            catch error:_ ->
                    %% Log a warning here using logger, to allow debugging the
                    %% highlighter?
                    ""
            end;
        false ->
            ""
    end.

use_highlight_in_context(Opts) ->
    case {proplists:get_value(fancy, Opts, true),
          proplists:get_value(forms, Opts),
          proplists:get_value(fmt_expr_fun, Opts)} of
        {true, Forms, undefined} when is_list(Forms) ->
            %% No formatting function is supplied, so we can use Erlang's
            %% parser, lexer and pretty-printer
            true;
        _No ->
            %% If we have pretty-printer, lexer and parser as options,
            %% we could print highlighted in context for e.g. Elixir too.
            false
    end.

highlight_in_context(AstNode, Opts) ->
    Color = use_color(Opts),
    [$\n, % blank line before
     case proplists:get_value(source_file, Opts, undefined) of
         undefined ->
             Forms = proplists:get_value(forms, Opts), % must be present here
             gradualizer_highlight:prettyprint_and_highlight(AstNode, Forms, Color);
         SourceFile ->
             {ok, SourceBin} = file:read_file(SourceFile),
             Source = binary_to_list(SourceBin),
             gradualizer_highlight:highlight_in_source(AstNode, Source, Color)
     end,
     $\n]. % blank line after

%% Determines if colors (escape sequences) should be used in the output.  If the
%% option for colors is 'auto' (the default), io:columns/0 is used to detect if
%% standard output is a TTY or not.
-spec use_color(gradualizer:options()) -> boolean().
use_color(Opts) ->
    case proplists:get_value(color, Opts, auto) of
        auto -> case io:columns() of {ok, _} -> true; _ -> false end;
        always -> true;                    % auto/always/never
        never -> false;                    % like for ls, grep, etc.
        Bool when is_boolean(Bool) -> Bool % bool (undocumented)
    end.

-spec describe_expr(gradualizer_type:abstract_expr()) -> io_lib:chars().
describe_expr({atom, _, _})               -> "atom";
describe_expr({bc, _, _, _})              -> "binary comprehension";
describe_expr({bin, _, _})                -> "bit expression";
describe_expr({block,_,_})                -> "block";
describe_expr({char, _, _})               -> "character";
describe_expr({call, _, _, _})            -> "function call";
describe_expr({'catch', _, _})            -> "catch expression";
describe_expr({'case', _, _, _})          -> "case expression";
describe_expr({cons, _, _, _})            -> "list";
describe_expr({float, _, _})              -> "float";
describe_expr({'fun', _, _})              -> "fun expression";
describe_expr({integer, _, _})            -> "integer";
describe_expr({'if', _, _})               -> "if expression";
describe_expr({lc, _, _, _})              -> "list comprehension";
describe_expr({map, _, _})                -> "map";
describe_expr({map, _, _, _})             -> "map update";
describe_expr({match, _, _, _})           -> "match";
describe_expr({named_fun, _, _, _})       -> "named fun expression";
describe_expr({nil, _})                   -> "empty list";
describe_expr({op, _, 'not', _})          -> "negation";
describe_expr({op, _, '-', _})            -> "negation";
describe_expr({op, _, Op, _, _})          -> io_lib:format("~w expression", [Op]);
describe_expr({record, _, _, _})          -> "record";
describe_expr({'receive', _, _, _, _})    -> "receive expression";
describe_expr({record, _, _, _, _})       -> "record update";
describe_expr({record_field, _, _, _, _}) -> "record field";
describe_expr({record_index, _, _, _})    -> "record index";
describe_expr({string, _, _})             -> "string";
describe_expr({tuple, _, _})              -> "tuple";
describe_expr({'try', _, _, _, _, _})     -> "try expression";
describe_expr({var, _, _})                -> "variable";
describe_expr(_)                          -> "expression".

pp_expr(Expr, Opts) ->
    case proplists:get_value(fmt_expr_fun, Opts) of
        Fun when is_function(Fun) ->
            Fun(Expr);
        _ ->
            erl_pp:expr(Expr)
    end.

pp_intersection_type([], _) ->
    "";
%% TODO: pp_type seems to have problems printing bounded types.
pp_intersection_type([{type, _, bounded_fun, [Ty, []]} | Tys], Opts) ->
    pp_type(Ty, Opts) ++ ["\n" || Tys /= []] ++ pp_intersection_type(Tys, Opts);
pp_intersection_type([Ty|Tys], Opts) ->
    pp_type(Ty, Opts) ++ ["\n" || Tys /= []] ++ pp_intersection_type(Tys, Opts).

pp_type(Ty, Opts) ->
    PP = case proplists:get_value(fmt_type_fun, Opts) of
             Fun when is_function(Fun) -> Fun(Ty);
             _                         -> typelib:pp_type(Ty)
         end,
    case use_color(Opts) of
        true  -> [?color_type, PP, ?color_end];
        false -> PP
    end.

print_errors(Errors, Opts) ->
    [print_error(Error, Opts) || Error <- Errors],
    ok.

print_error(Error, Opts) ->
    File = proplists:get_value(filename, Opts),
    FmtLoc = proplists:get_value(fmt_location, Opts, verbose),
    case File of
        undefined -> ok;
        _ when FmtLoc =:= brief -> io:format("~s:", [File]);
        _  -> io:format("~s: ", [File])
    end,
    handle_type_error(Error, Opts).

handle_type_error(Error, Opts) ->
    io:put_chars(gradualizer_fmt:format_type_error(Error, Opts)).
