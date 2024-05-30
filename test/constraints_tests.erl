-module(constraints_tests).

-include_lib("eunit/include/eunit.hrl").

%% Macro to convert type to abstract form
-define(t(T), t(??T)).
t(T) -> typelib:remove_pos(typelib:parse_type(T)).

validity_test() ->
    %% id(5)
    Env = gradualizer:env(),
    Cs0 = constraints:empty(),
    Cs1 = constraints:lower(a, ?t(5)),
    Cs2 = constraints:combine(Cs0, Cs1, Env),
    ?assertEqual(Cs1, Cs2),
    ?assert(constraints:satisfiable(Cs2, Env)).

validity2_test() ->
    %% lists:filter(fun even/1, [1, 2, 3])
    Env = gradualizer:env(),
    Cs0 = constraints:empty(),
    Cs1 = constraints:upper(a, ?t(integer())),
    Cs2 = constraints:lower(a, ?t(1)),
    Cs3 = constraints:lower(a, ?t(2)),
    Cs4 = constraints:lower(a, ?t(2)),
    Cs5 = constraints:combine([Cs0, Cs1, Cs2, Cs3, Cs4], Env),
    ?assert(constraints:satisfiable(Cs5, Env)).

validity3_test() ->
    %% lists:filter(fun not/1, [true, false])
    Env = gradualizer:env(),
    Cs0 = constraints:empty(),
    Cs1 = constraints:upper(a, ?t(boolean())),
    Cs2 = constraints:lower(a, ?t(true)),
    Cs3 = constraints:lower(a, ?t(false)),
    Cs4 = constraints:combine([Cs0, Cs1, Cs2, Cs3], Env),
    ?assert(constraints:satisfiable(Cs4, Env)).

validity4_test() ->
    %% lists:filter(fun not/1, [42])
    Env = gradualizer:env(),
    Cs0 = constraints:empty(),
    Cs1 = constraints:upper(a, ?t(boolean())),
    Cs2 = constraints:lower(a, ?t(42)),
    Cs3 = constraints:combine([Cs0, Cs1, Cs2], Env),
    ?assertMatch({false, a, _, _}, constraints:satisfiable(Cs3, Env)).

validity5_test() ->
    %% lists:filter(fun not/1, [true, maybe_true, false])
    Env = gradualizer:env(),
    Cs0 = constraints:empty(),
    Cs1 = constraints:upper(a, ?t(boolean())),
    Cs2 = constraints:lower(a, ?t(true)),
    Cs3 = constraints:lower(a, ?t(maybe_true)),
    Cs4 = constraints:lower(a, ?t(false)),
    Cs5 = constraints:combine([Cs0, Cs1, Cs2, Cs3, Cs4], Env),
    ?assertMatch({false, a, _, _}, constraints:satisfiable(Cs5, Env)).

validity6_test() ->
    %% lists:map(fun math:log/1, [1, 3.14])
    Env = gradualizer:env(),
    Cs0 = constraints:empty(),
    Cs1 = constraints:upper(a, ?t(number())),
    Cs2 = constraints:lower(b, ?t(float())),
    Cs3 = constraints:lower(a, ?t(1)),
    Cs4 = constraints:lower(a, ?t(float())),
    Cs5 = constraints:combine([Cs0, Cs1, Cs2, Cs3, Cs4], Env),
    ?assert(constraints:satisfiable(Cs5, Env)).
