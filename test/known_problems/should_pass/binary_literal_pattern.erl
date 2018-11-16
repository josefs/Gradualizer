-module(binary_literal_pattern).

-export([f/1]).

-type t1() :: binary().
-type maybe(T) :: undefined | T.

-spec f(maybe(t1())) -> ok | error | other.
f(T) ->
    case T of
        undefined -> error;
        %% The next line causes:
        %% The pattern <<"ok">> on line 14 doesn't have the type undefined | <<_:_*8>>
        <<"ok">> -> ok;
        _ -> other
    end.
