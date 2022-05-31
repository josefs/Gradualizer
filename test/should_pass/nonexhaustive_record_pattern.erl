-module(nonexhaustive_record_pattern).

-export([g/2]).

-record(env, {}).
-type env() :: #env{}.

-spec g(_, env()) -> ok.
g([], _Env) -> ok;
g([_Ty1|_Tys], _Env) -> ok.
