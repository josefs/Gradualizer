-module(undefined_user_type).
-export([j/0]).

-spec j() -> undefined_user_type_helper:j().
j() -> ok.
