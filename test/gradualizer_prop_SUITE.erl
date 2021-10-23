-module(gradualizer_prop_SUITE).

-compile([export_all, nowarn_export_all]).

-define(NUMTESTS, list_to_integer(os:getenv("PROP_NUMTESTS", "100"))).

%% Aliases
-define(gp, gradualizer_prop).

all() ->
    [
     remove_pos_removes_pos,
     normalize_type,
     glb,
     int_range_to_types,
     int_range_to_types_to_int_range,
     type_diff,
     refinable,
     compatible,
     type_check_expr,
     type_check_expr_in,
     type_check_forms
    ].

prop_opts() ->
    [{numtests, ?NUMTESTS},
     {constraint_tries, 50}].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    {ok, _} = application:ensure_all_started(gradualizer),
    Config.

end_per_testcase(_CaseName, Config) ->
    %% Clear gradualizer_db data between test runs.
    ok = application:stop(gradualizer),
    Config.

remove_pos_removes_pos(Config) ->
    check(?gp:prop_remove_pos_removes_pos(), prop_opts(), Config).

normalize_type(Config) ->
    check(?gp:prop_normalize_type(), prop_opts(), Config).

glb(Config) ->
    check(?gp:prop_glb(), prop_opts(), Config).

int_range_to_types(Config) ->
    check(?gp:prop_int_range_to_types(), prop_opts(), Config).

int_range_to_types_to_int_range(Config) ->
    Prop = ?gp:prop_int_range_to_types_to_int_range(),
    check(Prop, prop_opts(), Config).

type_diff(Config) ->
    check(?gp:prop_type_diff(), prop_opts(), Config).

refinable(Config) ->
    check(?gp:prop_refinable(), prop_opts(), Config).

compatible(Config) ->
    check(?gp:prop_compatible(), prop_opts(), Config).

type_check_expr(Config) ->
    check(?gp:prop_type_check_expr(), prop_opts(), Config).

type_check_expr_in(Config) ->
    check(?gp:prop_type_check_expr_in(), prop_opts(), Config).

type_check_forms(Config) ->
    check(?gp:prop_type_check_forms(), prop_opts(), Config).

%%
%% Helpers
%%

%% This comes as ct_property_test:quickcheck/2,
%% but the upstream version doesn't allow to pass options to the property test tool.
check(Property, Config) ->
    check(Property, [], Config).

check(Property, PropOpts, Config) ->
    Tool = proplists:get_value(property_test_tool,Config),
    F = function_name(quickcheck, Tool),
    mk_ct_return( Tool:F(Property, PropOpts), Tool ).

function_name(quickcheck, triq) -> check;
function_name(F, _) -> F.

mk_ct_return(true, _Tool) ->
    true;
mk_ct_return(Other, Tool) ->
    try lists:last(hd(Tool:counterexample()))
    of
        {set,{var,_},{call,M,F,Args}} ->
            {fail, io_lib:format("~p:~tp/~p returned bad result",[M,F,length(Args)])}
    catch
        _:_ ->
            {fail, Other}
    end.
