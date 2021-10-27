%% Integer types represented as ranges with infinities
-module(gradualizer_int).

%% Integer types
-export([is_int_type/1,
         is_int_subtype/2,
         int_type_glb/2,
         int_type_diff/2,
         negate_int_type/1,
         merge_int_types/1]).

%% Ranges <--> integer types
-export([int_type_to_range/1,
         int_range_to_type/1,
         int_range_to_types/1]).

%% Ranges
-export([int_range_diff/2]).

%% Types
-export_type([int_range/0]).

-type int() :: integer() | neg_inf | pos_inf.
-type int_range() :: {int(), int()}.
-type type() :: gradualizer_type:abstract_type().

%% +----------------------------------------+
%% |  Functions operating on integer types  |
%% +----------------------------------------+

%% Checks if a type is an integer type. This is a pre-condition for most of
%% the functions in this module.
%%
%% A macro with the same name is defined in typelib.hrl, which can be used in
%% guards.
-spec is_int_type(type()) -> boolean().
is_int_type({type, _, T, _})
  when T == pos_integer; T == non_neg_integer; T == neg_integer;
       T == integer; T == range -> true;
is_int_type({integer, _, _}) -> true;
is_int_type({char, _, _}) -> true;
is_int_type(_) -> false.

%% Checks if an integer type is a subtype of another integer type. Both
%% arguments must be integer types.
-spec is_int_subtype(type(), type()) -> boolean().
is_int_subtype(Ty1, Ty2) ->
    R1 = int_type_to_range(Ty1),
    R2 = int_type_to_range(Ty2),
    lower_bound_less_or_eq(R2, R1) andalso
        upper_bound_more_or_eq(R2, R1).

%% Greatest lower bound of two integer types.
int_type_glb(Ty1, Ty2) ->
    {Lo1, Hi1} = int_type_to_range(Ty1),
    {Lo2, Hi2} = int_type_to_range(Ty2),
    int_range_to_type({int_max(Lo1, Lo2), int_min(Hi1, Hi2)}).

%% Range difference, like set difference. The result may be zero, one or two
%% types.
-spec int_type_diff(type(), type()) -> type().
int_type_diff(Ty1, Ty2) ->
    IntRanges = int_range_diff(int_type_to_range(Ty1),
                               int_type_to_range(Ty2)),
    %% Make sure the result is a standard erlang type.
    %% Perhaps we can include generalized ranges such as 10..pos_inf
    ExpandedRanges = lists:map(fun int_range_expand_to_valid/1, IntRanges),
    int_ranges_to_type(ExpandedRanges).

%% Merges integer types by sorting on the lower bound and then merging adjacent
%% ranges. Returns a list of mutually exclusive integer types.
%%
%% This is an adoption of the standard algorithm for merging intervals.
-spec merge_int_types([type()]) -> [type()].
merge_int_types([]) ->
    [];
merge_int_types(IntTypes) ->
    Ranges = lists:map(fun int_type_to_range/1, IntTypes),
    int_ranges_to_types(Ranges).

%% Negates an integer type, e.g. `neg_integer() -> pos_integer()', `1..5 ->
%% -1..-5', etc.
negate_int_type(RangeTy) ->
    {L, U} = int_type_to_range(RangeTy),
    L2 = int_negate(U),
    U2 = int_negate(L),
    int_range_to_type({L2, U2}).

%% +---------------------------------------+
%% |  Conversion between types and ranges  |
%% +---------------------------------------+

%% Integer type to range.
-spec int_type_to_range(type()) -> int_range().
int_type_to_range({type, _, integer, []})              -> {neg_inf, pos_inf};
int_type_to_range({type, _, neg_integer, []})          -> {neg_inf, -1};
int_type_to_range({type, _, non_neg_integer, []})      -> {0, pos_inf};
int_type_to_range({type, _, pos_integer, []})          -> {1, pos_inf};
int_type_to_range({type, _, range, [{Tag1, _, I1}, {Tag2, _, I2}]})
  when Tag1 =:= integer orelse Tag1 =:= char,
       Tag2 =:= integer orelse Tag2 =:= char           -> {I1, I2};
int_type_to_range({char, _, I})                        -> {I, I};
int_type_to_range({integer, _, I})                     -> {I, I}.

%% Converts a range back to a type.
-spec int_range_to_type(int_range()) -> type().
int_range_to_type(Range) ->
    union(int_range_to_types(Range)).

%% +----------------+
%% |  Type helpers  |
%% +----------------+

%% Converts a range to a list of types. Creates two types in some cases and zero
%% types if lower bound is greater than upper bound.
-spec int_range_to_types(int_range()) -> [type()].
int_range_to_types({neg_inf, pos_inf}) ->
    [type(integer)];
int_range_to_types({neg_inf, -1}) ->
    [type(neg_integer)];
int_range_to_types({neg_inf, 0}) ->
    [type(neg_integer), {integer, erl_anno:new(0), 0}];
int_range_to_types({neg_inf, I}) when I > 0 ->
    [type(neg_integer),
     {type, erl_anno:new(0), range, [{integer, erl_anno:new(0), 0}
                                    ,{integer, erl_anno:new(0), I}]}];
int_range_to_types({neg_inf, I}) when I < -1 ->
    %% Non-standard
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), neg_inf}
                                    ,{integer, erl_anno:new(0), I}]}];
int_range_to_types({I, pos_inf}) when I < -1 ->
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), I}
                                    ,{integer, erl_anno:new(0), -1}]},
     type(non_neg_integer)];
int_range_to_types({-1, pos_inf}) ->
    [{integer, erl_anno:new(0), -1}, type(non_neg_integer)];
int_range_to_types({0, pos_inf}) ->
    [type(non_neg_integer)];
int_range_to_types({1, pos_inf}) ->
    [type(pos_integer)];
int_range_to_types({I, pos_inf}) when I > 1 ->
    %% Non-standard
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), I}
                                    ,{integer, erl_anno:new(0), pos_inf}]}];
int_range_to_types({I, I}) ->
    [{integer, erl_anno:new(0), I}];
int_range_to_types({I, J}) when is_integer(I) andalso
				is_integer(J) andalso
				I < J ->
    [{type, erl_anno:new(0), range, [{integer, erl_anno:new(0), I}
                                    ,{integer, erl_anno:new(0), J}]}];
int_range_to_types({pos_inf, _}) -> [];
int_range_to_types({_, neg_inf}) -> [];
int_range_to_types({I, J}) when I > J ->
    [].

%% Merges ranges and returns a single type (possibly a union).
-spec int_ranges_to_type([int_range()]) -> type().
int_ranges_to_type(Ranges) ->
    union(int_ranges_to_types(Ranges)).

%% Merges overlapping ranges and converts them to types.
-spec int_ranges_to_types([int_range()]) -> [type()].
int_ranges_to_types(Ranges) ->
    MergedRanges = merge_int_ranges(Ranges),
    lists:flatmap(fun int_range_to_types/1, MergedRanges).

-spec union([type()]) -> type().
union([]) -> type(none);
union([T]) -> T;
union(Ts) -> type(union, Ts).

type(Name) -> type(Name, []).
type(Name, Params) -> {type, erl_anno:new(0), Name, Params}.

%% +---------------------------------+
%% |  Functions operating on ranges  |
%% +---------------------------------+

%% Merges overlapping ranges and returns a sorted list of disjoint ranges.
-spec merge_int_ranges([int_range()]) -> [int_range()].
merge_int_ranges([]) ->
    [];
merge_int_ranges(Ranges) ->
    [T | Ts] = lists:sort(fun lower_bound_less_or_eq/2, Ranges),
    merge_int_ranges_help(Ts, [T]).

merge_int_ranges_help([{R1, R2} = R | Rs], [{S1, S2} | StackTail] = Stack) ->
    NewStack = if
                   R1 == neg_inf; S2 == pos_inf; R1 =< S2 + 1 ->
                       %% Overlapping or adjacent ranges. Merge them.
                       [{S1, int_max(R2, S2)} | StackTail];
                   true ->
                       %% Not mergeable ranges. Push R to stack.
                       [R | Stack]
               end,
    merge_int_ranges_help(Rs, NewStack);
merge_int_ranges_help([], Stack) ->
    lists:reverse(Stack).

%% Compares the lower bound of two ranges. Used as callback for sorting ranges.
-spec lower_bound_less_or_eq(int_range(), int_range()) -> boolean().
lower_bound_less_or_eq({A, _}, {B, _}) ->
    if
        A == neg_inf -> true;
        B == neg_inf -> false;
        true         -> A =< B
    end.

%% Compares the upper bound of two ranges.
-spec upper_bound_more_or_eq(int_range(), int_range()) -> boolean().
upper_bound_more_or_eq({_, A}, {_, B}) ->
    if
        A == pos_inf -> true;
        B == pos_inf -> false;
        true         -> A >= B
    end.

%% Computes the difference between two integer intervals and returns the result
%% as a list of zero, one or two intervals.
-spec int_range_diff(int_range(), int_range()) -> [int_range()].
int_range_diff({Lo1, Hi1}, {Lo2, Hi2}) ->
    %% R1:   xxxxxxxxxxxxxxxxxxxxx
    %% R2:         xxxxxxxxxxx
    %% diff: xxxxxx           xxxx

    Lo2x = int_decr(Lo2),
    Hi2x = int_incr(Hi2),

    BeforeR2 = [{Lo1, int_min(Hi1, Lo2x)} || int_less_than(Lo1, Lo2)],
    AfterR2  = [{int_max(Lo1, Hi2x), Hi1} || int_greater_than(Hi1, Hi2)],
    BeforeR2 ++ AfterR2.

%% Makes sure a range can be represented as a syntactically valid Erlang type,
%% by expanding it if necessary.
int_range_expand_to_valid({neg_inf, N}) when is_integer(N),
                                             N < -1 ->
    {neg_inf, -1}; % neg_integer()
int_range_expand_to_valid({N, pos_inf}) when is_integer(N),
                                             N > 1 ->
    {1, pos_inf}; % pos_integer()
int_range_expand_to_valid(Range) ->
    Range.

%% +-----------------------------------------+
%% |  Functions operating on a single int()  |
%% +-----------------------------------------+

int_min(A, B) when A == neg_inf; B == neg_inf   -> neg_inf;
int_min(pos_inf, B) -> B;
int_min(A, pos_inf) -> A;
int_min(A, B) when is_integer(A), is_integer(B) -> min(A, B).

int_max(A, B) when A == pos_inf; B == pos_inf   -> pos_inf;
int_max(neg_inf, B) -> B;
int_max(A, neg_inf) -> A;
int_max(A, B) when is_integer(A), is_integer(B) -> max(A, B).

int_less_than(A, A) -> false;
int_less_than(A, B) -> A =:= int_min(A, B).

int_greater_than(A, A) -> false;
int_greater_than(A, B) -> A =:= int_max(A, B).

int_incr(N) when is_integer(N) -> N + 1;
int_incr(Inf)                  -> Inf.

int_decr(N) when is_integer(N) -> N - 1;
int_decr(Inf)                  -> Inf.

int_negate(pos_inf) ->
    neg_inf;
int_negate(neg_inf) ->
    pos_inf;
int_negate(I) when is_integer(I) ->
    -I.
