-module(var_pattern_bound).

-export([var_inside_pattern/1]).

-spec var_inside_pattern(atom()) -> integer().
var_inside_pattern(Atom) ->
    case get_any() of
        {Atom} ->
            %% at this point still Atom :: atom()
            %% and not Atom :: any()
            Atom
    end.

-spec get_any() -> any().
get_any() ->
    receive Any -> Any end.
