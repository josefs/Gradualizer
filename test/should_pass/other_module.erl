-module(other_module).

-compile([export_all, nowarn_export_all]).

%% these exported types are used in remote_types module
-export_type([my_generic/1]).

% -type other_module_type() :: other_module.

%% v-- Either a local type (my_tuple) to this module or a remote user type
-type my_generic(RetType) :: my_shadowed_type() | {ok, RetType}.
%% A type to be shadowed by a type with the same name in a local module.
%% To be used in `user_types:my_generic(my_shadowed_type())`
-type my_shadowed_type() :: other_module.
