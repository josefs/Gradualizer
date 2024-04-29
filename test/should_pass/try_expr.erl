-module(try_expr).

-export([plain_try/2, plain_try_var/2, try_of/4, try_of_var/4]).

-spec may_fail(1 | 2 | 3) -> boolean().
may_fail(1) -> true;
may_fail(2) -> false;
may_fail(3) -> throw(got_three).

-spec plain_try(1 | 2 | 3, float()) -> boolean() | float().
plain_try(Key, Float) ->
    try may_fail(Key)
    catch
        got_three -> Float
    end.

-spec plain_try_var(1 | 2 | 3, float()) -> boolean() | float().
plain_try_var(Key, Float) ->
    X = try may_fail(Key)
    catch
        got_three -> Float
    end,
    X.

-spec try_of(1 | 2 | 3, float(), one, two) -> one | two | float().
try_of(Key, Float, OneAtom, TwoAtom) ->
    try may_fail(Key) of
        true -> OneAtom;
        false -> TwoAtom
    catch
        got_three -> Float
    end.

-spec try_of_var(1 | 2 | 3, float(), one, two) -> one | two | float().
try_of_var(Key, Float, OneAtom, TwoAtom) ->
    X = try may_fail(Key) of
        true -> OneAtom;
        false -> TwoAtom
    catch
        got_three -> Float
    end,
    X.
