-module(list_exhaustiveness_checking_regressions).

-export([f/2, g/1, h/1]).

f( Needle, [Needle | _]) -> ok;
f( Needle, [_ | Haystack]) -> f(Needle, Haystack);
f(_Needle, []) -> ok.

-spec g([a | b]) -> ok.
g([a | _Haystack]) -> ok;
g([_ | Haystack]) -> g(Haystack);
g([]) -> ok.

-spec h([a | b]) -> ok.
h([a | _Haystack]) -> ok;
h([]) -> ok;
h([_ | Haystack]) -> h(Haystack).
