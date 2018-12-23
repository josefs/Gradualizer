-module(nonempty_string_fail).

-export([empty_as_nonempty_string/0]).

-spec empty_as_nonempty_string() -> nonempty_string().
empty_as_nonempty_string() -> "".
