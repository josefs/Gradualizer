-module(branch2).

-export([c/1]).

-spec c(bool()) -> integer().
c(true) ->
    1;
c(false) ->
    apa.
