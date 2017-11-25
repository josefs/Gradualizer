-module(mfa).

-compile([export_all]).

mfa() ->
    epp:parse_forms("mfa.erl",[]).
