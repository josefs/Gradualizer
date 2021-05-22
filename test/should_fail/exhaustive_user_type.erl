-module(exhaustive_user_type).

-export([test/1]).

-type info() :: integer().

-type t() :: {true, info()}
           | {false, info()}.

-spec test(t()) -> ok.
test(T) ->
    case T of
        {true, _} -> ok
    end.
