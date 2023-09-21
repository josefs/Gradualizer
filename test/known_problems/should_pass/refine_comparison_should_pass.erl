-module(refine_comparison_should_pass).

-export([comp_map_value3/1]).

-type my_map() :: #{value := integer() | nil}.

-spec comp_map_value3(my_map()) -> integer().
comp_map_value3(State) when map_get(value, State) /= nil ->
    case State of
        #{value := Val} ->
            Val + 1
    end;
comp_map_value3(#{value := nil}) -> 0.
