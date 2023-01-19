-module(poly_maps_get_with_default_should_pass).

-gradualizer([solve_constraints]).

-export([f/0]).

%% -spec get(Key, Map, Default) -> Value | Default when
%%       Map :: #{Key => Value, _ => _}.
%%
%% Due to the way constraints from patterns are registered, Default is constrained to be a 2-tuple
%% even though we know it's matched out by the first case clause matching on the default_value atom.
%% This leads to:
%%
%% Lower bound default_value of type variable Default_typechecker_3529_3 on line 12
%% is not a subtype of
%% {Default_typechecker_3529_3_typechecker_1257_7, Default_typechecker_3529_3_typechecker_1257_8}
-spec f() -> ok.
f() ->
    case maps:get(k, #{}, default_value) of
        default_value ->
            ok;
        {_, _} ->
            ok
    end.
