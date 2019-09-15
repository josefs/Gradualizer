-module(exhaustive).

-export([integer_0/1,integer_1/1
	,pos_integer/1, neg_integer/1
	,tuple_1/1
	,union_atom/1, union_nil/1]).

-spec integer_0(integer()) -> {}.
integer_0(0) ->
    {}.

-spec integer_1(integer()) -> {}.
integer_1(1) ->
    {}.

-spec pos_integer(pos_integer()) -> {}.
pos_integer(1) ->
    {}.

-spec neg_integer(neg_integer()) -> {}.
neg_integer(-1) ->
    {}.

-spec tuple_1({integer()}) -> {}.
tuple_1({0}) ->
    {}.

-spec union_atom(a | b) -> {}.
union_atom(a) ->
    {}.

-spec union_nil([] | a) -> {}.
union_nil([]) ->
    {}.
