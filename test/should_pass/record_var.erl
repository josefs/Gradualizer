-module(record_var).

-export([f/1, g/0]).

-record(rec, {apa}).

-spec f(A) -> A when A :: #rec{}.
f(#rec{}) -> #rec{}.

%% also defined in records.erl - with different line numbers
-record(r, {f1     :: atom(),
            f2 = 1 :: integer()}).

%% The type of R is #r{} from the records module. As the record
%% definition and types are the same in the two modules the records
%% must be compatible.
g() ->
    R = records:h(),
    R#r.f1.
