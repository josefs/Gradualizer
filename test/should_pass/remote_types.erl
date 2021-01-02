-module(remote_types).

-compile([export_all, nowarn_export_all]).

-type local_tuple() :: {atom(), integer()}.
-type local_list() :: list(atom()).
-type local_union() :: float() | {atom(), integer()}.
-type my_mfa() :: {module(), atom(), arity()}.

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
