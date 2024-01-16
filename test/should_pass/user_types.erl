-module(user_types).

-compile([export_all, nowarn_export_all]).

%% these exported types are used in remote_types module
-export_type([my_tuple/0, my_list/0, my_union/0, my_opaque/0,
              my_empty_record/0, my_refined_record/0, my_generic/1]).

-record(r, {}).
-type my_empty_record() :: #r{}.
-record(refined_r, {a, b}).
-type my_refined_record() :: #refined_r{a :: integer(), b :: binary()}.
-type my_atom() :: atom().
-type my_tuple() :: {my_atom(), integer()}.
-type my_list() :: list(my_atom()).
-type my_union() :: float() | my_tuple().

%% v-- Either a local type (my_tuple) to this module or a remote user type
-type my_generic(RetType) :: my_shadowed_type() | {ok, RetType}.
%% A type to be shadowed by a type with the same name in a local module.
%% i.e. `user_types:my_generic(my_shadowed_type())` where `my_shadowed_type()`
%% is defined in the module using the remoty type `my_generic` above.
-type my_shadowed_type() :: user_types.

-opaque my_opaque() :: integer().

-spec f(my_tuple()) -> any().
f({A, _}) ->
    A.

-spec new_opaque() -> my_opaque().
new_opaque() ->
    0.

-spec update_opaque(my_opaque()) -> my_opaque().
update_opaque(N) ->
    N + 1.
