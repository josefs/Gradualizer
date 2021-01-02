-module(pattern).

-export([pattern_test/1]).

-spec pattern_test(integer()) -> {}.
pattern_test(1) ->
    true;
pattern_test(_) ->
    {}.
