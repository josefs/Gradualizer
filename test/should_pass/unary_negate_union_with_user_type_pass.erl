-module(unary_negate_union_with_user_type_pass).

-type price() :: integer().

-spec price(_) -> float() | price().
price(_) -> 100.

-spec f(_, _) -> boolean().
f(G, ReversePaymentG) ->
    price(G) =:= - price(ReversePaymentG).
