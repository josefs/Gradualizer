-module(sets_set).

-spec add_var(string(), sets:set(string())) -> sets:set(string()).
add_var(Var, Set) ->
    sets:add_element(Var, Set).
