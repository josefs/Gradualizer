-module(imported_undef).

-export([foo/0]).
-import(any, [undef_foo/0]).

-spec foo() -> ok.
foo() -> undef_foo().
