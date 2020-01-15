-module(intersection_exhaustive).

-export([f/1]).

-spec f(a) -> b;
       (b) -> a.
f(a) -> b;
f(b) -> a.  % "The clause cannot be reached"
