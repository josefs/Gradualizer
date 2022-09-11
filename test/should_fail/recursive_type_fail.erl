-module(recursive_type_fail).

-export([g/0, j/0, m/0]).

-type recursive_t1() :: #{key => recursive_t1()}.

-type recursive_t2() :: #{binary() | atom() => recursive_t2()} | true | false | null.

-type recursive_t3() :: #{binary() | atom() => recursive_t3()}
                      | true | false | null
                      | integer() | float()
                      | binary() | atom()
                      | calendar:datetime().

-spec f() -> recursive_t1().
f() ->
    g().

-spec g() -> recursive_t1().
g() ->
    [].

-spec i() -> recursive_t2().
i() ->
    j().

-spec j() -> recursive_t2().
j() ->
    [].

-spec l() -> recursive_t3().
l() ->
    m().

-spec m() -> recursive_t3().
m() ->
    [].
