-module(type_refinement_3).
-export([imprecision_prevents_refinement/2]).

-spec imprecision_prevents_refinement(float(), a|b) -> b.
imprecision_prevents_refinement(3.14, a) -> b;
imprecision_prevents_refinement(_, X) -> X.
