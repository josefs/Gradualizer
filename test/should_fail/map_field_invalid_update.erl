-module(map_field_invalid_update).

-export([f/1, g/1]).

-spec f(#{a := integer()}) -> #{a := binary()}.
f(#{} = Ctx) ->
    Ctx#{a := not_a_binary}.

%% Ctx might not have a field `a`, but Ctx2 has for sure (mandatory)
-spec g(map()) -> #{a := any()}.
g(Ctx) ->
    Ctx2 = Ctx#{a => 5},
    Ctx2.
