-module(depth).

-export([bar3/0, bar4/0]).

-spec bar3() -> {{{0}}}.
bar3() -> {{{1}}}.

%%% not detected by Dialyzer
-spec bar4() -> {{{{0}}}}.
bar4() -> {{{{1}}}}.
