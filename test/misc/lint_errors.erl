%% This module doesn't even compile. The errors here are caught by erl_lint.
-module(lint_errors).
-export([local_type/0,
         local_call/0,
         one_more_for_the_record/0,
         local_record/1,
         record_field/1,
         illegal_binary_segment/1,
         invalid_record_info/0,
         illegal_pattern/1]).

-spec local_type() -> undefined_type().
local_type() -> ok.

-spec local_call() -> ok.
local_call() -> undefined_call().

-record(r, {a :: #s{}}).

%% The number of expected errors are the number of exported functions,
%% so we create a function without errors, to account for the error in
%% the record definition above.
one_more_for_the_record() -> ok.

-spec local_record(#r{}) -> boolean().
local_record(R) -> if
    (R#r.a)#s.a == c -> true;
    true -> false
   end.

-spec record_field(#r{}) -> boolean().
record_field(R) -> if
    R#r.b == c -> true;
    true -> false
   end.

illegal_binary_segment(X) ->
    <<X:42/utf8>>. %% Size not allowed with utf8/16/32

invalid_record_info() -> record_info(foo, bar).

-spec illegal_pattern(gradualizer:top()) -> gradualizer:top().
illegal_pattern(1 + A) -> ok.
