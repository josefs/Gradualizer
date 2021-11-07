-module(remote_types).

-compile([export_all, nowarn_export_all]).

-type local_tuple() :: {atom(), integer()}.

%% test resolving remote tuple referencing a user type in the remote module
-spec f(local_tuple()) -> user_types:my_tuple().
f(A) -> A.
