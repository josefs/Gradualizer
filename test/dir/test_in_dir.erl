-module(test_in_dir).

-export([fail/1]).

-spec fail(integer()) -> atom().
fail(N) -> N.
