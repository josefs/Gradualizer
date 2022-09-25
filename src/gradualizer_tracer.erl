%% @doc A utility module to make troubleshooting problems
%% by tracing Gradualizer internals more efficient.
%% Configuration of this module is compile time, but using the tracing facilities is more efficient
%% than traditional printf-debugging anyway.

-module(gradualizer_tracer).

-export([start/0,
         flush/0,
         debug/1]).

-include("typechecker.hrl").

-compile([{nowarn_unused_function, [{just_tenv, 1},
                                    {just_venv, 1},
                                    {skip_env, 1}]}]).

%% @doc This is the trace function that the tracer will use.
%% Customize it as you see fit, then recompile.
trace_fun() ->
    fun
        %% Here are some examples of what might come in handy when troubleshooting
        %% typechecker internals.
        %% For example, it's possible to only print the actual types in calls to `compat/4'
        %% and `compat_ty/4', but skip the lengthy `Seen' and `Env' arguments.
        %% Just uncomment the below:
        %%
        %% ({trace, _Pid, call, {_M, _F = compat, [_Ty1, _Ty2, _, _]}}, ok) ->
        %%     Trace = {trace, _Pid, call, {_M, _F, [_Ty1, _Ty2, seen, env]}},
        %%     io:format("~p\n", [Trace]);
        %% ({trace, _Pid, call, {_M, _F = compat_ty, [_Ty1, _Ty2, _, _]}}, ok) ->
        %%     Trace = {trace, _Pid, call, {_M, _F, [_Ty1, _Ty2, seen, env]}},
        %%     io:format("~p\n", [Trace]);
        %%
        %% In a similar fashion we can modify the return values from `subtype/2',
        %% `compat/4' etc:
        %%
        %% ({trace, _Pid, return_from, {_M, _F = subtype, _Arity}, {Ret, _Constraints}}, ok) ->
        %%     Trace = {trace, _Pid, return_from, {_M, _F, _Arity}, {Ret, constraints}},
        %%     io:format("~p\n", [Trace]);
        %% ({trace, _Pid, return_from, {_M, _F = compat, _Arity}, {_Ret, _Constraints}}, ok) ->
        %%     Trace = {trace, _Pid, return_from, {_M, _F, _Arity}, {true, constraints}},
        %%     io:format("~p\n", [Trace]);
        %% ({trace, _Pid, return_from, {_M, _F = compat_ty, _Arity}, {_Ret, _Constraints}}, ok) ->
        %%     Trace = {trace, _Pid, return_from, {_M, _F, _Arity}, {true, constraints}},
        %%     io:format("~p\n", [Trace]);

        %% In the general case, however, it might be more convenient to use one of the already
        %% available helpers like `just_venv/1' or `skip_env/1' in the clauses below:
        ({trace, _Pid, call, {_M, _F, _Args}}, ok) when is_list(_Args) ->
            Trace = {trace, _Pid, call, {_M, _F, simplify(_Args)}},
            io:format("~p\n", [Trace]);

        ({trace, _Pid, return_from, {_M, _F, _Arity}, RetVal}, ok) ->
            RV = case RetVal of
                     _ when is_list(RetVal) ->
                         simplify(RetVal);
                     _ when is_tuple(RetVal) ->
                         list_to_tuple(simplify(tuple_to_list(RetVal)));
                     _ ->
                         RetVal
                 end,
            Trace = {trace, _Pid, return_from, {_M, _F, _Arity}, RV},
            io:format("~p\n", [Trace]);

        (Trace, ok) ->
            io:format("~p\n", [Trace])
    end.

simplify(Args) ->
    %skip_env(Args).
    just_venv(Args).
    %just_tenv(Args).

skip_env(Args) ->
    lists:map(fun
                  (Arg) when element(1, Arg) =:= env -> env;
                  (Arg) -> Arg
              end, Args).

just_venv(Args) ->
    lists:map(fun
                  (Arg) when element(1, Arg) =:= env ->
                      {venv, Arg#env.venv};
                  (Arg) -> Arg
              end, Args).

just_tenv(Args) ->
    lists:map(fun
                  (Arg) when element(1, Arg) =:= env ->
                      {tenv, Arg#env.tenv};
                  (Arg) -> Arg
              end, Args).

%% @doc Start tracing.
start() ->
    {ok, Tracer} = dbg:tracer(process, {trace_fun(), ok}),
    %dbg:p(all, [call, arity, return_to]),
    dbg:p(all, [call, return_to]),

    %dbg:tpl(typechecker, type_check_function, x),
    %dbg:tpl(typechecker, check_clauses_fun, x),
    %dbg:tpl(typechecker, check_clauses, x),
    %dbg:tpl(typechecker, check_clause, x),
    %dbg:tpl(typechecker, refine_vars_by_mismatching_clause, x),
    %dbg:tpl(typechecker, check_arg_exhaustiveness, x),

    %dbg:tpl(typechecker, check_arg_exhaustiveness, x),
    %dbg:tpl(typechecker, exhaustiveness_checking, x),
    %dbg:tpl(typechecker, all_refinable, x),
    %dbg:tpl(typechecker, no_clause_has_guards, x),
    %dbg:tpl(typechecker, some_type_not_none, x),

    %dbg:tpl(typechecker, check_clause, x),
    %dbg:tpl(typechecker, check_clause, x),
    %dbg:tpl(typechecker, add_types_pats, x),
    %dbg:tpl(typechecker, check_guards, x),
    %dbg:tpl(typechecker, add_var_binds, x),
    %dbg:tpl(typechecker, type_check_block_in, x),
    %dbg:tpl(typechecker, refine_clause_arg_tys, x),
    %dbg:tpl(typechecker, refine_mismatch_using_guards, x),

    %dbg:tpl(typechecker, expect_tuple_type, x),
    %dbg:tpl(typechecker, expect_tuple_union, x),
    %dbg:tpl(typechecker, refine_clause_arg_tys, x),
    %dbg:tpl(typechecker, refine_ty, x),

    %dbg:tpl(typechecker, add_types_pats, 4, x),
    %dbg:tpl(typechecker, add_types_pats, 6, x),
    %dbg:tpl(typechecker, add_type_pat, 3, x),
    %dbg:tpl(typechecker, add_type_pat_union, 3, x),
    %dbg:tpl(typechecker, denormalize, x),
    %dbg:tpl(typechecker, type_check_block_in, x),

    %dbg:tpl(typechecker, type_check_expr_in, x),
    %dbg:tpl(typechecker, do_type_check_expr_in, x),
    %dbg:tpl(typechecker, type_check_logic_op_in, x),

    %dbg:tpl(typechecker, subtype, x),
    %dbg:tpl(typechecker, compat, x),
    %dbg:tpl(typechecker, compat_seen, x),
    %dbg:tpl(typechecker, compat_ty, x),

    %dbg:tpl(typechecker, glb_ty, x),
    %dbg:tpl(typechecker, normalize, x),
    %dbg:tpl(typechecker, do_add_types_pats, x),

    %dbg:tpl(?MODULE, debug, x),
    %dbg:tpl(erlang, throw, x),

    application:set_env(gradualizer, tracer, Tracer),
    ok.

%% @doc `debug/1' is a trace point to trace when pinpointing issues across several candidate
%% locations. Uncomment the below in `start/0':
%%
%%   dbg:tpl(?MODULE, debug, x)
%%
%% Then insert the following somewhere in code, recompile and check the trace for the line from
%% which `debug/1' was called:
%%
%%   gradualizer_tracer:debug(?LINE)
%%
debug(_) -> ok.

%% @doc Wait for flushing all the trace messages to stdout.
flush() ->
    case application:get_env(gradualizer, tracer, no_tracer) of
        no_tracer -> ok;
        Tracer when is_pid(Tracer) ->
            timer:sleep(100)
    end.
