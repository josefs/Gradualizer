-module(remote_types_pass).

-type options() :: proplists:proplist().

-spec f(options()) -> ok.
f(Opts) ->
    OptsForModule = h() ++ Opts,
    g(OptsForModule).

-spec g(proplists:proplist()) -> ok.
g(_Opts) -> ok.

-spec h() -> options().
h() -> [].
