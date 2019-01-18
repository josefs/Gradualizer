-module(map_field_invalid_update).

-export([f/1]).

-type map_with_integer() :: #{a := integer()}.
-type map_with_binary()  :: #{a := binary()}.

-spec f(map_with_integer()) -> map_with_binary().
f(#{} = Ctx) ->
    Ctx#{a := not_a_binary}.
