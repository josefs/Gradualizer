-module(tuple).

-compile([export_all, nowarn_export_all]).

-spec tup({any(),any(),any()}) -> any().
tup({A,_,_}) ->
    A.

tup2(A,B) ->
    {A,B}.

-type tup() :: {atom(), integer()}.
-spec tup3(atom(), integer()) -> tup().
tup3(A,B) ->
    {A,B}.

%% expect_tuple_type should be able to extract that the type `any() | atom()` expects (accepts) any tuple
%% (There is a shortcut for f(any()) which avoids this code path)
tup4(A) ->
    f({A, 1}).

-spec f(any() | atom()) -> ok.
f(_) ->
    ok.
