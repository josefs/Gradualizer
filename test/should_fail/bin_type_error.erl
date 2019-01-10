-module(bin_type_error).

-export([throws_bin_type_error/1]).

-spec throws_bin_type_error(integer()) -> any().
throws_bin_type_error(<<>>) -> ok.
