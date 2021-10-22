-module(gradualizer_type_gen).

-export([abstract_type/0, abstract_type/1,
         expr/0, expr/1]).

abstract_type() ->
    Opts = [{weight, {binop, 0}},
            {weight, {unop, 0}},
            {weight, {remote_type, 0}},
            {weight, {user_defined_type, 0}}],
    abstract_type(Opts).

abstract_type(Opts) ->
    State = gradualizer_erlang_abstract_code:options(Opts),
    gradualizer_erlang_abstract_code:abstract_type(State).

expr() ->
    Exclude = [%% Due to some strange reason, maps or other full blown types get generated
               %% as bitstring segments
               bitstring,
               %% Possibly remote function calls
               termcall, varcall, localcall, extcall,
               %% Remote fun refs
               ext_mfa, any_mfa],
    ExcludeWeights = [ {weight, {Tag, 0}} || Tag <- Exclude ],
    Opts = ExcludeWeights,
    expr(Opts).

expr(Opts) ->
    gradualizer_erlang_abstract_code:expr(Opts).
