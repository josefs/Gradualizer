%%% @doc Main external API of the Gradualizer
%%%
%%% The functions `type_check(file|module|dir)' accept the following options:
%%% - `stop_on_first_error': if `true' stop type checking at the first error,
%%%   if `false' continue checking all functions in the given file and all files
%%%   in the given directory.
%%% - `print_file': if `true' prefix error printouts with the file name the
%%%   error is from.
-module(gradualizer).

-include("typelib.hrl").

-export([type_check_file/1,
         type_check_file/2,
         type_check_module/1,
         type_check_module/2,
         type_check_dir/1,
         type_check_dir/2,
         type_check_files/1,
         type_check_files/2,
         format_error/1,
         format_errors/2
        ]).

%% API functions

%% @doc Type check a source or beam file
-spec type_check_file(file:filename()) -> [typechecker:type_error()].
type_check_file(File) ->
    type_check_file(File, []).

%% @doc Type check a source or beam file
-spec type_check_file(file:filename(), typechecker:options()) -> [typechecker:type_error()].
type_check_file(File, Opts) ->
    ParsedFile =
        case filename:extension(File) of
            ".erl" ->
                gradualizer_file_utils:get_forms_from_erl(File);
            ".beam" ->
                gradualizer_file_utils:get_forms_from_beam(File);
            Ext ->
                throw({unknown_file_extension, Ext})
        end,
    case ParsedFile of
        {ok, Forms} ->
            case typechecker:type_check_forms(Forms, Opts) of
                [] -> [];
                Errors ->
                    NoReportErrors = proplists:get_bool(no_report_errors, Opts),
                    if NoReportErrors -> Errors;
                       true ->
                            PrintFile = proplists:get_bool(print_file, Opts),
                            io:format("~s", [format_errors(PrintFile,
                                                           [{File, Errors}])]),
                            Errors
                    end
            end;
        Error ->
            throw(Error)
    end.


%% @doc Type check a module
-spec type_check_module(module()) -> [typechecker:type_error()].
type_check_module(Module) ->
    type_check_module(Module, []).

%% @doc Type check a module
-spec type_check_module(module(), typechecker:options()) -> [typechecker:type_error()].
type_check_module(Module, Opts) when is_atom(Module) ->
    case code:which(Module) of
        File when is_list(File) ->
            type_check_file(File, Opts);
        Error when is_atom(Error) ->
            throw({beam_not_found, Error})
    end.

%% @doc Type check all source or beam files in a directory.
-spec type_check_dir(file:filename()) -> [{file:filename(), [typechecker:type_error()]}].
type_check_dir(Dir) ->
    type_check_dir(Dir, []).

%% @doc Type check all source or beam files in a directory.
-spec type_check_dir(file:filename(), typechecker:options()) -> [{file:filename(), [typechecker:type_error()]}].
type_check_dir(Dir, Opts) ->
    case filelib:is_dir(Dir) of
        true ->
            type_check_files(filelib:wildcard(filename:join(Dir, "*.{erl,beam}")), Opts);
        false ->
            throw({dir_not_found, Dir})
    end.

%% @doc Type check a source or beam file
-spec type_check_files([file:filename()]) -> [{file:filename(), [typechecker:type_error()]}].
type_check_files(Files) ->
    type_check_files(Files, []).

%% @doc Type check a source or beam
-spec type_check_files([file:filename()], typechecker:options()) -> [{file:filename(), [typechecker:type_error()]}].
type_check_files(Files, Opts) ->
    StopOnFirstError = proplists:get_bool(stop_on_first_error, Opts),
    lists:foldl(
        fun(File, Errors) when Errors =:= [];
                            not StopOnFirstError ->
                case type_check_file(File, Opts) of
                    [] -> Errors;
                    NewErrors -> [{File, NewErrors} | Errors]
                end;
            (_, Errors) ->
                Errors
        end, [], Files).

-spec format_error(typechecker:type_error()) -> io_lib:chars().
format_error(#{error_type := {undef, call},
               line       := LINE,
               name       := Func,
               arity      := Arity}) ->
    io_lib:format("Call to undefined function ~p/~p on line ~p~n",
              [Func, Arity, LINE]);
format_error(#{error_type := {undef, call},
               line       := LINE,
               module     := Module,
               name       := Func,
               arity      := Arity}) ->
    io_lib:format("Call to undefined function ~p:~p/~p on line ~p~n",
                  [Module, Func, Arity, LINE]);
format_error(#{error_type := {undef, record},
               line       := LINE,
               module     := Module,
               name       := RecName}) ->
    io_lib:format("Undefined record ~p:~p on line ~p~n",
                  [Module, RecName, LINE]);
format_error(#{error_type := {undef, record},
               line      := LINE,
               name      := RecName}) ->
    io_lib:format("Undefined record ~p on line ~p~n",
                  [RecName, LINE]);
format_error(#{error_type := {undef, Type},
               line       := LINE,
               module     := Module,
               name       := Name,
               arity      := Arity})
               when Type =:= user_type;
                    Type =:= remote_type ->
    io_lib:format("Undefined ~p ~p:~p/~p on line ~p~n",
                  [Type, Module, Name, Arity, LINE]);
format_error(#{error_type := {undef, user_type},
               line       := LINE,
               name       := Name,
               arity      := Arity}) ->
    io_lib:format("Undefined user type ~p/~p on line ~p~n",
                  [Name, Arity, LINE]);
format_error(#{error_type := unexported_remote_type,
               line       := LINE,
               module     := Module,
               name       := Name,
               arity      := Arity}) ->
    io_lib:format("The type ~s:~s/~p on line ~p is not exported~n",
                  [erl_pp:expr(Module), erl_pp:expr(Name), Arity, LINE]);
format_error(#{error_type    := var,
               line          := LINE,
               name          := Var,
               actual_type   := VarTy,
               expected_type := Ty}) ->
    io_lib:format("The variable ~p on line ~p has type ~s "
                  "but is expected to have type ~s~n",
                  [Var, LINE, typelib:pp_type(VarTy), typelib:pp_type(Ty)]);
format_error(#{error_type    := char,
               line          := LINE,
               expression    := Char,
               expected_type := Ty}) ->
    io_lib:format("The character ~p on line ~p does not have type ~s~n",
                  [erl_pp:expr(Char), LINE, typelib:pp_type(Ty)]);
format_error(#{error_type    := atom,
               line          := LINE,
               expression    := A,
               expected_type := Ty}) ->
    io_lib:format("The atom ~p on line ~p does not have type ~s~n",
                  [A, LINE, typelib:pp_type(Ty)]);
format_error(#{error_type    := string,
               line          := LINE,
               expression    := String,
               expected_type := Ty}) ->
    io_lib:format("The string ~p on line ~p does not have type ~s~n",
                  [String, LINE, typelib:pp_type(Ty)]);
format_error(#{error_type    := int,
               line          := LINE,
               expression    := I,
               expected_type := Ty}) ->
    io_lib:format("The integer ~p on line ~p does not have type ~s~n",
                 [I, LINE, typelib:pp_type(Ty)]);
format_error(#{error_type    := float,
               line          := LINE,
               expression    := F,
               expected_type := Ty}) ->
    io_lib:format("The float ~p on line ~p does not have type ~s~n",
                  [F, LINE, typelib:pp_type(Ty)]);
format_error(#{error_type  := type_error,
               expression  := {cons, LINE, _, _},
               actual_type := Ty}) ->
    io_lib:format("The type ~s on line ~p is not a list type~n",
                  [typelib:pp_type(Ty), LINE]);
format_error(#{error_type    := cons_pat,
               line          := P,
               expression    := Cons,
               expected_type := Ty}) ->
    io_lib:format("The pattern ~s on line ~p does not have type:~n~s~n",
                  [erl_pp:expr(Cons),P, typelib:pp_type(Ty)]);
format_error(#{error_type    := cons,
               line          := P,
               expression    := Cons,
               expected_type := Ty}) ->
    io_lib:format("The expression ~s on line ~p does not have type ~s~n",
                  [erl_pp:expr(Cons), P, typelib:pp_type(Ty)]);
format_error(#{error_type    := nil,
               line          := LINE,
               expected_type := Ty}) ->
    io_lib:format("The empty list on line ~p does not have type ~s~n",
                  [LINE, typelib:pp_type(Ty)]);
format_error(#{error_type     := argument_length_mismatch,
               line           := P,
               arity          := LenArgs,
               expected_arity := LenTy}) ->
    io_lib:format("The clause on line ~p is expected to have ~p argument(s) "
                  "but it has ~p~n ",
                  [P, LenTy, LenArgs]);
format_error(#{error_type     := {call, arity},
               line           := P,
               expression     := Fun,
               expected_arity := TyArity,
               arity          := CallArity}) ->
    io_lib:format("The function ~s at line ~p expects ~p argument~s, but is given ~p~n",
              [erl_pp:expr(Fun), P, TyArity, ["s" || TyArity /= 1], CallArity]);
format_error(#{error_type    := {call, args},
               line          := P,
               name          := Name,
               actual_type   := ArgTys,
               expected_type := TyArgs}) ->
    io_lib:format("The function ~p expects arguments of type~n~p~n"
                  "but the call on line ~p gives it arguments of type~n~p~n",
                  [P, Name, TyArgs, ArgTys]);
format_error(#{error_type  := {call, context},
               line        := P,
               name        := Name,
               actual_type := FunTy}) ->
    io_lib:format("The type of the function ~s, called on line ~p doesn't match "
                  "the surrounding calling context.~n"
                  "It has the following type~n~s~n",
                  [erl_pp:expr(Name), P, typelib:pp_intersection_type(FunTy)]);
format_error(#{error_type    := mfa,
               line          := P,
               module        := M,
               name          := F,
               arity         := A,
               actual_type   := FunTy,
               expected_type := ResTy}) ->
    io_lib:format("The mfa ~p:~p/~p on line ~p is expected to have type : ~n~s~n"
                  "but has type : ~n"
                  "~s~n",
                  [M, F, A, P,typelib:pp_type(ResTy),
                  typelib:pp_intersection_type(FunTy)]);
format_error(#{error_type    := return,
               line          := P,
               name          := Name,
               actual_type   := FunResTy,
               expected_type := ResTy}) ->
    io_lib:format("The function ~s on line ~p is expected to return: ~s~n"
                  "but it returns: ~s~n",
                  [Name, P, typelib:pp_type(ResTy), typelib:pp_type(FunResTy)]);
format_error(#{error_type  := expected_fun_type,
               line        := P,
               name        := Name,
               actual_type := FunTy}) ->
    io_lib:format("Expected function ~s on line ~p to have a function type,~n"
                  "but it has the following type:~n~s~n",
                  [Name, P, typelib:pp_type(FunTy)]);
format_error(#{error_type  := intersection_mismatch,
               line        := P,
               name        := Name,
               actual_type := FunTy}) ->
    io_lib:format("None of the types of the function ~s at line ~p matches the "
                  "call site. Here's the types of the function:~n~s~n",
                  [Name, P, typelib:pp_intersection_type(FunTy)]);
format_error(#{error_type  := boolop,
               line        := P,
               expression  := BoolOp,
               actual_type := Ty}) ->
    io_lib:format("The operator ~p on line ~p is given a non-boolean argument "
                  "of type ~s~n",
                  [BoolOp, P, typelib:pp_type(Ty)]);
format_error(#{error_type := relop,
               line         := P,
               expression  := RelOp,
               actual_type  := {Ty1, Ty2}}) ->
    io_lib:format("The operator ~p on line ~p requires arguments of "
                  "compatible types.~nHowever, it has arguments "
                  "of type ~s and ~s~n",
                  [RelOp, P, typelib:pp_type(Ty1), typelib:pp_type(Ty2)]);
format_error(#{error_type    := op_type_too_precise,
               line          := P,
               expression    := Op,
               expected_type := Ty})
              when ?is_int_type(Ty) ->
    io_lib:format("The operator ~p on line ~p is expected to have type "
                  "~s which is not a supertype of float()~n",
                  [Op, P, typelib:pp_type(Ty)]);
format_error(#{error_type    := op_type_too_precise,
               line          := P,
               expression    := Op,
               expected_type := Ty}) ->
    io_lib:format("The operator ~p on line ~p is expected to have type "
                  "~s which is too precise to be statically checked~n",
                  [Op, P, typelib:pp_type(Ty)]);
format_error(#{error_type  := {arith_op, arguments},
               line        := P,
               expression  := ArithOp,
               actual_type := {Ty1, Ty2}}) ->
    io_lib:format("The operator ~p on line ~p is requires numeric arguments, "
                  "but has arguments of type ~s and ~s~n",
                  [ArithOp, P, typelib:pp_type(Ty1), typelib:pp_type(Ty2)]);
format_error(#{error_type  := {int_op, arguments},
               line        := P,
               expression  := ArithOp,
               actual_type := {Ty1, Ty2}}) ->
    io_lib:format("The operator ~p on line ~p is requires integer arguments, "
                  "but has arguments of type ~s and ~s~n",
                  [ArithOp, P, typelib:pp_type(Ty1), typelib:pp_type(Ty2)]);
format_error(#{error_type    := {arith_op, expression},
               line          := P,
               expression    := ArithOp,
               expected_type := Ty}) ->
    io_lib:format("The operator ~p on line ~p is expected to have type "
                  "~s which has no numeric subtypes~n",
                  [ArithOp, P, typelib:pp_type(Ty)]);
format_error(#{error_type    := {int_op, expression},
               line          := P,
               expression    := IntOp,
               expected_type := Ty}) ->
    io_lib:format("The operator ~p on line ~p is expected to have type "
                  "~s which has no integer subtypes~n",
                  [IntOp, P, typelib:pp_type(Ty)]);
format_error(#{error_type  := type_error,
               expression  := {op, P, '+', _},
               actual_type := Ty}) ->
    io_lib:format("The plus expression on line ~p has a non-numeric argument "
                  "of type:~n~s~n",
                  [P, typelib:pp_type(Ty)]);
format_error(#{error_type  := type_error,
               expression  := {op, P, '-', _},
               actual_type := Ty}) ->
    io_lib:format("The negated expression on line ~p has a "
                  "non-numeric argument of type:~n~s~n",
                  [P, typelib:pp_type(Ty)]);
format_error(#{error_type  := type_error,
               expression  := {op, P, 'not', _},
               actual_type := Ty}) ->
    io_lib:format("The 'not' expression on line ~p has a non-boolean argument "
                  "of type ~s~n",
                  [P, typelib:pp_type(Ty)]);
format_error(#{error_type    := unary_op,
               line          := P,
               expression    := Op,
               actual_type   := TargetTy,
               expected_type := Ty}) ->
    io_lib:format("The application of unary '~s' on line ~p is expected to "
                  "have type ~s, which has no shared subtype with ~s~n",
                  [Op, P, typelib:pp_type(Ty), typelib:pp_type(TargetTy)]);
format_error(#{error_type  := type_error,
               expression  := {op, P, 'bnot', _},
               actual_type := Ty}) ->
    io_lib:format("The 'bnot' expression on line ~p has a non-integer argument "
                  " of type ~s~n",
                  [P, typelib:pp_type(Ty)]);
format_error(#{error_type    := logic_op,
               line          := P,
               expression    := LogicOp,
               expected_type := Ty})
                when LogicOp == 'andalso';
                     LogicOp == 'orelse' ->
    Target = if LogicOp == 'andalso' -> false;
                LogicOp == 'orelse'  -> true end,
    io_lib:format("The operator ~p on line ~p is expected to have type "
                  "~s which does not include '~p'~n",
                  [LogicOp, P, typelib:pp_type(Ty), Target]);
format_error(#{error_type    := logic_op,
               line          := P,
               expression    := LogicOp,
               expected_type := Ty}) ->
    io_lib:format("The operator ~p on line ~p is expected to have type "
                  "~s which is not a supertype of boolean()~n",
                  [LogicOp, P, typelib:pp_type(Ty)]);
format_error(#{error_type    := {rel_op, expression},
               line          := P,
               expression    := LogicOp,
               expected_type := Ty}) ->
    io_lib:format("The operator ~p on line ~p is expected to have type "
                  "~s which is not a supertype of boolean()~n",
                  [LogicOp, P, typelib:pp_type(Ty)]);
format_error(#{error_type := {rel_op, arguments},
               line         := P,
               expression   := LogicOp,
               actual_type  := {Ty1, Ty2}}) ->
    io_lib:format("The operator ~p on line ~p is given two arguments with "
                  "non-compatible types:~n~s~n~s~n",
                  [LogicOp, P, typelib:pp_type(Ty1), typelib:pp_type(Ty2)]);
format_error(#{error_type    := {list_op, expression},
               line          := P,
               expression    := ListOp,
               expected_type := Ty}) ->
    io_lib:format("The operator ~p on line ~p is expected to have type "
                  "~s, which has no list subtypes~n",
                  [ListOp, P, typelib:pp_type(Ty)]);
format_error(#{error_type  := {list_op, arguments},
               line        := P,
               expression  := ListOp,
               actual_type := Ty}) ->
    io_lib:format("The operator ~p on line ~p is given an argument "
                  "with a non-list type ~s~n",
                  [ListOp, P, typelib:pp_type(Ty)]);
format_error(#{error_type    := operator_pattern,
               line          := P,
               expression    := Expr,
               expected_type := Ty}) ->
    io_lib:format("The operator pattern ~s on line ~p is expected to have type "
                  "~s~n",
                  [erl_pp:expr(Expr), P, typelib:pp_type(Ty)]);
format_error(#{error_type    := pattern,
               line          := P,
               expression    := Pat,
               expected_type := Ty}) ->
    io_lib:format("The pattern ~s on line ~p doesn't have the type ~s~n",
                  [erl_pp:expr(Pat), P, typelib:pp_type(Ty)]);
format_error(#{error_type    := tuple,
               line          := P,
               expected_type := Ty}) ->
    io_lib:format("The tuple on line ~p does not have type ~s~n",
                  [P, typelib:pp_type(Ty)]);
format_error(#{error_type := {undef, variable},
               line       := P,
               expression := Var}) ->
    io_lib:format("Unknown variable ~p on line ~p.~n", [Var, P]);
format_error(#{error_type    := {bit, expression},
               line          := P,
               actual_type   := ActualTy,
               expected_type := ExpectTy}) ->
    io_lib:format("The bit expression on line ~p is expected "
                  "to have type ~s but it has type ~s~n",
                  [erl_anno:line(P),
                   typelib:pp_type(ExpectTy),
                   typelib:pp_type(ActualTy)]);
format_error(#{error_type    := {bit, arguments},
               line          := P,
               expression    := Expr,
               actual_type   := Ty1,
               expected_type := Ty2}) ->
    io_lib:format("The expression ~s inside the bit expression on line ~p has "
                  "type ~s but the type specifier indicates ~s~n",
                  [erl_pp:expr(Expr),
                   erl_anno:line(P),
                   typelib:pp_type(Ty1),
                   typelib:pp_type(Ty2)]);
format_error(#{error_type    := generator,
               line          := P,
               expected_type := Ty}) ->
    io_lib:format("The generator in a list comprehension on line ~p is "
                  "expected to return a list type, but returns ~s~n",
                  [erl_anno:line(P), typelib:pp_type(Ty)]);
format_error(#{error_type    := bin_generator,
               line          := P,
               expected_type := Ty}) ->
    io_lib:format("The binary generator on line ~p is expected "
                  "to return a bitstring type, but returns ~s~n",
                  [erl_anno:line(P), typelib:pp_type(Ty)]);
format_error(#{error_type    := {comprehension, bit_string},
               line          := P,
               expression    := Expr,
               expected_type := Ty}) ->
    io_lib:format("The expression ~s in the bit string comprehension on "
                  "line ~p has type ~s but a bit type is expected.~n",
                  [erl_pp:expr(Expr), erl_anno:line(P), typelib:pp_type(Ty)]);
format_error(#{error_type := check_clauses}) ->
    %%% TODO: Improve quality of type error
    "Type error in clauses";
format_error(#{error_type    := type_error,
               expression    := {record, P, Record, _},
               expected_type := ResTy}) ->
    io_lib:format("The record #~p on line ~p is expected to have type ~s.~n",
                  [Record, P, typelib:pp_type(ResTy)]);
format_error(#{error_type    := type_error,
               expression    := {record_field, P, _, Record, {atom, _, Field}},
               actual_type   := Ty,
               expected_type := ExpectTy}) ->
    io:format("The record field #~p.~p on line ~p has type ~s but is expected to have type ~s.~n"
             ,[Record, Field, P, typelib:pp_type(Ty), typelib:pp_type(ExpectTy)]);
format_error(#{error_type    := record_pattern,
               line          := P,
               expression    := Record,
               expected_type := Ty}) ->
    io_lib:format("The record patterns for record #~p on line ~p is expected "
                  "to have type ~s.~n",
                  [Record, P, typelib:pp_type(Ty)]);
format_error(#{error_type    := record_update,
               line          := P,
               expression    := Record,
               expected_type := ResTy}) ->
    io_lib:format("The record update of the record #~p on line ~p is expected "
                  "to have type:~n~s~n",
                  [Record, P, typelib:pp_type(ResTy)]);
format_error(#{error_type := receive_after,
               line          := P,
               actual_type   := {TyClauses, TyBlock}}) ->
    io_lib:format("The types in the clauses and the after block are incompatible~n"
                  "in the receive statement on line ~p.~n"
                  "The type of the clauses is : ~s~n"
                  "The type of the after block is : ~s~n",
                  [erl_anno:line(P),
                   typelib:pp_type(TyClauses),
                   typelib:pp_type(TyBlock)]);
format_error(#{error_type    := {comprehension, list},
               line          := P,
               expected_type := Ty}) ->
    io_lib:format("The list comprehension at line ~p is expected to have type "
                  "~s which has no list subtypes~n",
                  [P, typelib:pp_type(Ty)]);
format_error(#{error_type    := {comprehension, binary},
               line          := P,
               expected_type := Ty}) ->
    io_lib:format("The binary comprehension at line ~p is expected to have type "
                  "~s which has no binary subtypes~n",
                  [P, typelib:pp_type(Ty)]);
format_error(#{error_type    := lambda,
               line          := P,
               expected_type := Ty}) ->
    io_lib:format("The function expression at line ~p is expected to have type "
                  "~s which is not a function type~n",
                  [P, typelib:pp_type(Ty)]);
format_error(#{error_type  := cyclic_type_vars,
               line        := _P,
               actual_type := Ty,
               expression  := Xs}) ->
    io_lib:format("The type spec ~s has a cyclic dependency in variable~s ~s~n",
                  [typelib:pp_type(Ty),
                   [ "s" || length(Xs) > 1 ],
                   string:join(lists:map(fun atom_to_list/1, lists:sort(Xs)),
                               ", ")]);
format_error(#{error_type    := mismatch,
               line          := Line,
               expression    := Expr,
               expected_type := Ty}) ->
     io_lib:format("The expression ~s at line ~p does not have type ~s~n",
                   [erl_pp:expr(Expr), Line, typelib:pp_type(Ty)]).

-spec format_errors(boolean(), [{file:filename(), [typechecker:type_error()]}]) -> io_lib:chars().
format_errors(true, ErrorsPerFile) ->
    lists:flatmap(
        fun({Filename, Errors}) ->
            lists:map(
                fun(Error) ->
                    io_lib:format("~s: ~s", [Filename, format_error(Error)])
                end,
                Errors)
        end, ErrorsPerFile);
format_errors(false, ErrorsPerFile) ->
    lists:flatmap(
        fun({_, Errors}) ->
            lists:map(fun format_error/1, Errors)
        end, ErrorsPerFile).
