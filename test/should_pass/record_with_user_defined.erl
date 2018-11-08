-module(record_with_user_defined).
-export([foo/2]).

-type pred() :: fun((term()) -> boolean()).

-record(state, {
		pred   :: pred()}).

-spec foo(term(), #state{}) -> boolean().
foo(Event, State) ->
    #state{ pred = Pred} = State,
Pred(Event).
