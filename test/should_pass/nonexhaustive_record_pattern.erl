-module(nonexhaustive_record_pattern).

-export([g/2]).

-record(env, {}).
-type env() :: #env{}.

-spec g(integer(), env()) -> ok.
g(1, _Env) -> ok;
g(_I, _Env) -> ok.
