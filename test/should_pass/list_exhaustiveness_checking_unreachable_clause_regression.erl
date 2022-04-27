-module(list_exhaustiveness_checking_unreachable_clause_regression).

-export([f/2]).

f( Needle, [Needle | _]) -> ok;
f( Needle, [_ | Haystack]) -> f(Needle, Haystack);
f(_Needle, []) -> ok.
