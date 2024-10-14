-module(binary_exhaustiveness_checking_should_pass).

-export([f/0]).

-spec f() -> ok.
f() ->
    Cond = <<"asd">>,
    case Cond of
        <<_/bytes>> -> ok
    end.
