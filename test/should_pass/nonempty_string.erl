-module(nonempty_string).

-export([foo/0]).

-spec foo() -> nonempty_string().
foo() -> "banana".
