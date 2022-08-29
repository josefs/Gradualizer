-module(nonexhaustive_record_pattern).

-export([f/2,
         g/2]).

-record(env, {}).
-type env() :: #env{}.

-spec f(_, env()) -> ok.
f([], _Env) -> ok;
f([_Ty1|_Tys], _Env) -> ok.

-spec g(integer(), env()) -> ok.
g(1, _Env) -> ok;
g(_I, _Env) -> ok.
