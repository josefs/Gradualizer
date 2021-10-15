-module(infinite_loop4).

-export([unwrap1/1,
         unwrap2/1]).

-type rec1(A) :: A | rec1({A | rec1(A)}).

%% The original infinite loop example defined this as:
%%
%%   -spec unwrap(rec1(A)) -> ok.
%%   unwrap1({qwe, zxc}) -> ok.
%%
%% However, it doesn't compile due to: type variable 'A' is only used once (is unbound)
%%
%% The following are two variants of the above:
%% - when the pattern matches the type used for A
%% - and when it doesn't
-spec unwrap1(rec1(integer())) -> ok.
unwrap1(_) -> ok.

-spec unwrap2(rec1(integer())) -> ok.
unwrap2({qwe, zxc}) -> ok.
