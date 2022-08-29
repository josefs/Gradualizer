-module(lc_var_binds_in_filters).

-export([f/0]).

-spec f() -> [ok].
f() ->
    [ ok
      || begin
             Id = 1,
             true
         end,
         Id * 2 > 0 ].
