-module(return_fun_should_fail).

-export([return_fun_no_spec/0]).

-spec return_fun_no_spec() -> integer().
return_fun_no_spec() -> fun no_spec/0.

no_spec() -> ok.
