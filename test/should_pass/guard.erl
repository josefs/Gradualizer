-module(guard).

-compile([export_all, nowarn_export_all]).

-spec atom(atom() | integer()) -> atom() | not_atom.
atom(A) when is_atom(A) -> A;
atom(_) -> not_atom.

-spec atom2(atom() | integer()) -> atom() | not_atom.
atom2(A) when erlang:is_atom(A) -> A;
atom2(_) -> not_atom.

-spec binary(binary() | atom()) -> binary() | not_binary.
binary(B) when is_binary(B) -> B;
binary(_) -> not_binary.

-spec bitstring(bitstring() | atom()) -> bitstring() | not_bitstring.
bitstring(B) when is_bitstring(B) -> B;
bitstring(_) -> not_bitstring.

-spec boolean(boolean() | atom()) -> boolean() | not_boolean.
boolean(B) when is_boolean(B) -> B;
boolean(_) -> not_boolean.

-spec float(float() | atom()) -> float() | not_float.
float(F) when is_float(F) -> F;
float(_) -> not_float.

-spec integer(integer() | atom()) -> integer() | not_integer.
integer(I) when is_integer(I) -> I;
integer(_) -> not_integer.

-spec number(number() | atom()) -> number() | not_number.
number(N) when is_number(N) -> N;
number(_) -> not_number.

-spec list(list() | atom()) -> list() | not_list.
list(L) when is_list(L) -> L;
list(_) -> not_list.

-spec map(map() | atom()) -> map() | not_map.
map(M) when is_map(M) -> M;
map(_) -> not_map.

-spec pid(pid() | atom()) -> pid() | not_pid.
pid(P) when is_pid(P) -> P;
pid(_) -> not_pid.

-spec port(port() | atom()) -> port() | not_port.
port(P) when is_port(P) -> P;
port(_) -> not_port.

-spec reference(reference() | atom()) -> reference() | not_reference.
reference(R) when is_reference(R) -> R;
reference(_) -> not_reference.

-spec tuple(tuple() | atom()) -> tuple() | not_tuple.
tuple(T) when is_tuple(T) -> T;
tuple(_) -> not_tuple.

-spec function(function() | atom()) -> function() | not_function.
function(F) when is_function(F) -> F;
function(_) -> not_function.

-spec function2(fun((any()) -> any()) | atom()) -> fun((any()) -> any()) | not_function.
function2(F) when is_function(F, 1) -> F;
function2(_) -> not_function.

-record(r, {
    f
}).

-spec record(#r{} | atom()) -> #r{} | not_r.
record(R) when is_record(R, r) -> R;
record(_) -> not_r.

-spec record2(#r{} | atom()) -> #r{} | not_r.
record2(R) when is_record(R, r, 1) -> R;
record2(_) -> not_r.

-spec multiple(integer() | float() | atom()) -> integer() | float() | not_number.
multiple(I) when is_integer(I) -> I;
multiple(F) when is_float(F) -> F;
multiple(_) -> not_number.

-spec or_test(integer() | float() | atom()) -> integer() | float() | not_number.
or_test(N) when is_integer(N) or is_float(N) -> N;
or_test(_) -> not_number.

-spec orelse1(integer() | float() | atom()) -> integer() | float() | not_number.
orelse1(N) when is_integer(N) orelse is_float(N) -> N;
orelse1(_) -> not_number.

-spec orelse2(integer() | float() | atom()) -> integer() | float() | not_number.
orelse2(N) when is_integer(N); is_float(N) -> N;
orelse2(_) -> not_number.

-spec and_test(integer() | atom()) -> integer() | not_integer.
and_test(N) when is_integer(N) and is_number(N) -> N;
and_test(_) -> not_integer.

-spec andalso1(integer() | atom()) -> integer() | not_integer.
andalso1(N) when is_integer(N) andalso is_number(N) -> N;
andalso1(_) -> not_integer.

-spec andalso2(integer() | atom()) -> integer() | not_integer.
andalso2(N) when is_integer(N), is_number(N) -> N;
andalso2(_) -> not_integer.

-spec good_andalso(integer() | atom(), integer() | atom()) -> integer().
good_andalso(X, Y) when is_integer(X) andalso is_integer(Y) -> X + Y;
good_andalso(_, _) -> 42.

-spec guard_chain(integer() | binary()) -> integer().
guard_chain(I) when is_binary(I), size(I) > 3 ->
    3;
guard_chain(I) ->
    I.

-spec guard_on_two_vars(integer() | binary(), atom()) -> integer().
guard_on_two_vars(I, J) when is_binary(I), is_atom(J) ->
    3;
guard_on_two_vars(I, _) ->
    I.

-spec all(All) -> All when
    All :: atom()
        | binary()
        | bitstring()
        | boolean()
        | float()
        | function()
        | integer()
        | list()
        | map()
        | number()
        | pid()
        | port()
        | #r{}
        | reference()
        | tuple().
all(X) when is_atom(X) -> X;
all(X) when is_binary(X) -> X;
all(X) when is_bitstring(X) -> X;
all(X) when is_boolean(X) -> X;
all(X) when is_float(X) -> X;
all(X) when is_function(X) -> X;
all(X) when is_function(X, 1) -> X;
all(X) when is_integer(X) -> X;
all(X) when is_list(X) -> X;
all(X) when is_map(X) -> X;
all(X) when is_number(X) -> X;
all(X) when is_pid(X) -> X;
all(X) when is_port(X) -> X;
all(X) when is_record(X, r) -> X;
all(X) when is_record(X, r, 1) -> X;
all(X) when is_reference(X) -> X;
all(X) when is_tuple(X) -> X.
