-module(intersection_with_any_fail).

-export([any_refined_using_guard/1,
         intersection_using_constraits/1,
         var_as_pattern/1,
         var_inside_pattern/1]).

%% X :: any() & atom() by refinement
-spec any_refined_using_guard(any()) -> 5.
any_refined_using_guard(X) when is_atom(X) ->
    X.

%% X :: integer() & any() by the constraints
-spec intersection_using_constraits(X) when X :: integer(),
                                            X :: any() ->
                                                      atom().
intersection_using_constraits(X) ->
    X.

-spec var_as_pattern(atom()) -> integer().
var_as_pattern(Atom) ->
    case get_any() of
        Atom ->
	    %% at this point Atom :: any()
	    %% but we want Atom :: atom() & any()
            Atom
    end.

-spec var_inside_pattern(atom()) -> integer().
var_inside_pattern(Atom) ->
    case get_any() of
        {Atom} ->
            %% at this point Atom :: any()
            %% but we want Atom :: atom() & any()
            Atom
    end.

%% helper
-spec get_any() -> any().
get_any() ->
    receive Any -> Any end.
