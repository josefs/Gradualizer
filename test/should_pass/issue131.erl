-module(issue131).
-export([f/0]).
-spec f() -> #{ a := b } | undefined.
f() -> #{ a => b }.
