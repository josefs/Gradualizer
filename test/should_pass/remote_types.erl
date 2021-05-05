-module(remote_types).

-compile([export_all, nowarn_export_all]).

-type local_tuple() :: {atom(), integer()}.
-type local_list() :: list(atom()).
-type local_union() :: float() | {atom(), integer()}.
-type my_mfa() :: {module(), atom(), arity()}.

%% Shadows `my_shadowed_type` from user_types.erl
-type my_shadowed_type() :: remote_types.
%% Shadows `my_opaque` from user_types.erl
-opaque my_opaque() :: remote_types.

%% test resolving remote tuple referencing a user type in the remote module
-spec f(local_tuple()) -> user_types:my_tuple().
f(A) -> A.

%% test resolving remote list referencing a user type in the remote module
-spec g(local_list()) -> user_types:my_list().
g(A) -> A.

%% test resolving remote union referencing a user type in the remote module
-spec h(local_union()) -> user_types:my_union().
h(A) -> A.

%% test resolving built-in aliases within built-in aliases
-spec i(my_mfa()) -> mfa().
i(A) -> A.

%% test resolving/matching opaque remote types
-spec opaque_user() -> user_types:my_opaque().
opaque_user() ->
    user_types:new_opaque().

%% When resolving a user_type in a generic, the local-type should be used, never the remote type
%% if a remote type with the same name exists
%% In the test below, all three modules (user_types, other_module, and this one remote_types)
%% use the same type `my_shadowed_type` defined as the name of their respective modules.
-spec local_type_with_same_name_as_remote_type(atom()) -> user_types:my_generic(other_module:my_generic(my_shadowed_type())).
local_type_with_same_name_as_remote_type('1') -> user_types;
local_type_with_same_name_as_remote_type('2') -> {ok, other_module};
local_type_with_same_name_as_remote_type(_) -> {ok, {ok, remote_types}}.

%% Just as above, this should resolve the opaque type from this module
-spec generic_remote_opaque() -> user_types:my_generic(my_opaque()).
generic_remote_opaque() -> {ok, remote_types}.

-spec local_type_nested_generic(atom()) -> other_module:nested_generic(my_shadowed_type()).
local_type_nested_generic('1') -> nested_generic;
local_type_nested_generic('2') -> other_module;
local_type_nested_generic(_) -> {ok, remote_types}.
