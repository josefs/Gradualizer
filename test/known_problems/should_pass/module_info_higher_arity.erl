-module(module_info_higher_arity).

-export([two_plus_four/0, module_info/2]).

% It should still be possible to define and call
% custom module_info for arities other than 0 and 1.
% Note that this function has to be exported to make
% it possible to call it by its fully quallified name,
% and thus we have to introduce a type error in it,
% so that Gradualizer test suite does not complain.
-spec module_info(binary(), number()) -> number().
module_info(A, B) -> A + B.

-spec two_plus_four() -> number().
% This call fails with 'Call to undefined function ...'.
two_plus_four() -> module_info_higher_arity:module_info(2, 4).