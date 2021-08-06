-module(map_field_valid_update).

-export([f/1, g/1, h/1, i/1, j/1]).

-spec f(#{a := boolean()}) -> #{a := integer()}.
f(#{} = Ctx) ->
    Ctx#{a := 5}.

%% Ctx might not have a field `a`, but Ctx2 has for sure (mandatory)
-spec g(map()) -> #{a := any(), _ => _}.
g(Ctx) ->
    Ctx2 = Ctx#{a => 5},
    Ctx2.

%% When updating map() it can still have any other optional key
-spec h(map()) -> #{a := any(), b => any()}.
h(Ctx) ->
    Ctx2 = Ctx#{a => 5},
    Ctx2.

-spec i(#{a := any(), b => any(), c => any()})
       -> #{a := any(), b => any(), c := any(), d := any()}.
i(Ctx) ->
    Ctx2 = Ctx#{c => 1, d => 4},
    Ctx2.

-spec j(#{}) -> #{a := any(), b => any()}.
j(Ctx) ->
    Ctx2 = Ctx#{a => 5},
    Ctx2.
