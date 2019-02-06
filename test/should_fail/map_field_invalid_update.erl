-module(map_field_invalid_update).

-export([f/1, g/1]).

-spec f(#{a := integer()}) -> #{a := binary()}.
f(#{} = Ctx) ->
    Ctx#{a := not_a_binary}.

-spec g(#{}) -> #{a := any(), b => any()}.
g(Ctx) ->
    Ctx2 = Ctx#{a => 5},
    Ctx2.

