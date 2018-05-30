-module(tuple_union).

-compile(export_all).

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% Unions of tuples
%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


%%% not detected
%%% According to OTP, {A,B} | {C, D} is treated like {A|C, B|D}.
-spec tuple_union() -> {undefined, binary()} | {integer(), undefined}.
tuple_union() ->
    {undefined, undefined}.
