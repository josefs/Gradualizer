-module(map_pattern_fail).
-export([f/1,
         badkey/1,
         map_term/1,
         not_a_map_passed_as_map/0,
         wrong_spec/1
        ]).

-type t() :: #{apa := integer(), bepa := boolean()}.

-spec f(t()) -> string().
f(#{bepa := Bepa}) ->
    Bepa.

-spec badkey(#{apa => atom()}) -> ok.
badkey(#{bepa := _Bepa}) -> ok.

-spec map_term(gradualizer:top()) -> any().
map_term(#{k := V}) ->
    %% at this point V :: term()
    atom_to_list(V).

not_a_map_passed_as_map() ->
    G = g(),
    h(G).

-spec g() -> {tup, le}.
g() -> {tup, le}.

-spec h(map()) -> ok.
h(#{k := v} = _Map) ->
    ok.

-spec wrong_spec(tuple()) -> ok.
wrong_spec(#{k := v} = _Map) ->
    ok.
