-module(branch2).

-export([c/1]).

-spec c(boolean()) -> integer().
c(true) ->
    1;
c(false) ->
    apa.
