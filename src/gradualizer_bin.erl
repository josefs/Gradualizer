%% Helper module for binaries/bitstrings
-module(gradualizer_bin).

-export([compute_type/1]).

-include("gradualizer.hrl").

%% Computes the type of a bitstring expression or pattern based on the sizes
%% of the elements. The returned type is a normalized bitstring type.
-spec compute_type(ExprOrPat) -> gradualizer_type:abstract_type()
        when ExprOrPat :: {bin, _, _},
             ExprOrPat :: gradualizer_type:abstract_expr().
compute_type(Bin) ->
    View = bin_view(Bin),
    bitstr_view_to_type(View).

%% <<_:B, _:_*U>> is represented as {B, U} (fixed base + multiple of unit)
-type bitstr_view() :: {non_neg_integer(), non_neg_integer()} | none.

bitstr_concat({B1, U1}, {B2, U2}) ->
    {B1 + B2, gcd(U1, U2)};
bitstr_concat(none, _) -> none;
bitstr_concat(_, none) -> none.

-spec bitstr_view_to_type(bitstr_view()) -> gradualizer_type:abstract_type().
bitstr_view_to_type({B, U}) ->
    Anno = erl_anno:new(0),
    {type, Anno, binary, [{integer, Anno, B}, {integer, Anno, U}]};
bitstr_view_to_type(none) ->
    {type, erl_anno:new(0), none, []}.

%% Returns the view of a bit expression or pattern, i.e. computes its size
-spec bin_view({bin, _, _}) -> bitstr_view().
bin_view({bin, _, BinElements}) ->
    ElementViews = [bin_element_view(E) || E <- BinElements],
    lists:foldl(fun bitstr_concat/2, {0, 0}, ElementViews).

bin_element_view({bin_element, Anno, {Lit, _, _}, default, _Spec} = BinElem)
  when Lit == integer; Lit == char; Lit == string ->
    %% Literal with default size, i.e. no variables to consider.
    %% Size is not allowed for utf8/utf16/utf32.
    Bin = {bin, Anno, [BinElem]},
    {value, Value, []} = safe_eval_expr(Anno, Bin, {on_error, throw}),
    {bit_size(?assert_type(Value, bitstring())), 0};
bin_element_view({bin_element, Anno, {string, _, Chars}, Size, Spec}) ->
    %% Expand <<"ab":32/float>> to <<$a:32/float, $b:32/float>>
    %% FIXME: Not true for float, integer
    Views = [bin_element_view({bin_element, Anno, {char, Anno, Char}, Size, Spec})
             || Char <- Chars],
    lists:foldl(fun bitstr_concat/2, {0, 0}, Views);
bin_element_view({bin_element, _Anno, _Expr, default, Specifiers}) ->
    %% Default size
    %% <<1/integer-unit:2>> gives the following error:
    %% * 1: a bit unit size must not be specified unless a size is specified too
    %% However <<(<<9:9>>)/binary-unit:3>> gives no error.
    %% The type specifier 'binary' seems to be the only exception though.
    case get_type_specifier(Specifiers) of
        integer   -> {8, 0};
        float     -> {64, 0};
        binary    -> {0, get_unit(Specifiers)};
        bytes     -> {0, 8};
        bitstring -> {0, 1};
        bits      -> {0, 1};
        utf8      -> {0, 8};  %% 1-4 bytes
        utf16     -> {0, 16}; %% 2-4 bytes
        utf32     -> {32, 0}  %% 4 bytes, fixed
    end;
bin_element_view({bin_element, Anno, _Expr, SizeSpec, Specifiers}) ->
    %% Non-default size, possibly a constant expression
    case safe_eval_expr(Anno, SizeSpec, {on_error, return}) of
        {value, Sz, _VarBinds} when is_integer(Sz) ->
            {Sz * get_unit(Specifiers), 0};
        {error, {unbound_var, _}} ->
            %% Variable size
            U = get_unit(Specifiers),
            case get_type_specifier(Specifiers) of
                float when U == 64 -> {64, 0};  %% size must be 1 in this case
                float              -> {32, 32}; %% a float must be 32 or 64 bits
                _OtherType         -> {0, U}    %% any multiple of the unit
            end;
        _ ->
            throw({illegal_binary_segment, Anno, SizeSpec})
    end.

safe_eval_expr(Anno, Expr, {on_error, Action}) ->
    try
        erl_eval:expr(Expr, [])
    catch
        error:Reason ->
            case Action of
                throw ->
                    throw({illegal_binary_segment, Anno, Expr});
                return ->
                    {error, Reason}
            end
    end.

-spec get_type_specifier(Specifiers :: [atom() | {unit, non_neg_integer()}] |
                                       default) -> atom().
get_type_specifier(Specifiers) when is_list(Specifiers) ->
    case [S || S <- Specifiers,
               S == integer orelse S == float orelse
                 S == binary orelse S == bytes orelse
                 S == bitstring orelse S == bits orelse
                 S == utf8 orelse S == utf16 orelse
                 S == utf32] of
        [S|_] -> ?assert_type(S, atom());
        []    -> integer   %% default
    end;
get_type_specifier(default) -> integer.

get_unit(Specifiers) when is_list(Specifiers) ->
    case [U || {unit, U} <- Specifiers] of
        [U|_] -> U;
        [] -> get_default_unit(Specifiers)
    end;
get_unit(default) -> 1.

get_default_unit(Specifiers) when is_list(Specifiers) ->
    case get_type_specifier(Specifiers) of
        binary -> 8;
        bytes  -> 8;
        _Other -> 1
    end.

-spec gcd(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
gcd(A, B) when B > A -> gcd1(B, A);
gcd(A, B)            -> gcd1(A, B).

-spec gcd1(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
gcd1(A, 0) -> A;
gcd1(A, B) ->
  case A rem B of
    0 -> B;
    X when X > 0 -> gcd1(B, X)
  end.
