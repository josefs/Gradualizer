-module(user_types).

-export([f/1]).

%% these exported types are used in remote_types module
-export_type([my_tuple/0, my_list/0, my_union/0, my_opaque/0, my_empty_record/0]).

-record(r, {}).
-type my_empty_record() :: #r{}.
-type my_atom() :: atom().
-type my_tuple() :: {my_atom(), integer()}.
-type my_list() :: list(my_atom()).
-type my_union() :: float() | my_tuple().

-opaque my_opaque() :: integer().

-spec f(my_tuple()) -> any().
f({A, _}) ->
    A.

-spec new_opaque() -> my_opaque().
new_opaque() ->
    0.
