-module(gradualizer_type_gen).

-export([abstract_type/0, abstract_type/1,
         expr/0, expr/1,
         module/0, module/1]).

abstract_type() ->
    Opts = [{weight, {binop, 0}},
            {weight, {unop, 0}},
            {weight, {remote_type, 0}}],
    abstract_type(Opts).

abstract_type(Opts) ->
    State = gradualizer_erlang_abstract_code:options(Opts),
    gradualizer_erlang_abstract_code:abstract_type(State).

expr() ->
    Exclude = [%% TODO: Due to some strange reason, maps or other full blown types get generated
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

module() ->
    %% See expr() generator.
    Exclude = [bitstring,
               termcall, varcall, localcall, extcall,
               ext_mfa, any_mfa,
               record_field_access],
    %% TODO: define records before enabling their generation again
    ExcludeWeights = [ {weight, {Tag, 0}} || Tag <- Exclude ],
    %% The generator might generate function clauses with different number of params.
    %% This makes typechecking fail and is not expected in real world.
    %% TODO: Limit to just one-clause functions.
    Limits = [{limit, {function_clauses, 1}}],
    Opts = ExcludeWeights ++ Limits,
    module(Opts).

module(Opts) ->
    gradualizer_erlang_abstract_code:module(Opts).
