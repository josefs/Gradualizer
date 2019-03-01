-module(pattern_ann_type).

-export([f/1]).

-type mytuple() :: tuple().

%% The problem is that a user_type/remote_type is encapsulated in an
%% ann_type. The type is not normalized if annotated and
%% `expect_tuple_type/2` cannot handle user/remote types.
%% Type check passes if Pat annotation is removed.
-spec f(Pat :: mytuple()) -> ok.
f({}) ->
    ok.
