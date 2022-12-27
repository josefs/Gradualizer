-module(recursive_call_with_remote_union_return_type_should_pass).

-export([recursive/0]).

-spec recursive() -> ok | user_types:my_union().
recursive() ->
    recursive().
