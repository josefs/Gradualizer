-module(gradualizer_type_gen).

-export([abstract_type/0, abstract_type/1]).

abstract_type() ->
    DefaultOpts = [{weight, {remote_type, 0}},
                   {weight, {user_defined_type, 0}}],
    abstract_type(DefaultOpts).

abstract_type(Opts) ->
    State = gradualizer_erlang_abstract_code:options(Opts),
    gradualizer_erlang_abstract_code:abstract_type(State).
