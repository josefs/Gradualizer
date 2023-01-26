-module(any_doesnt_have_type_none_pass).

-export([f/2]).

-type type() :: abstract_type().

-type abstract_type() :: af_tuple_type().

-type anno() :: erl_anno:anno().

-type af_tuple_type() :: {'type', anno(), 'tuple', 'any'}
                       | {'type', anno(), 'tuple', [abstract_type()]}.

-spec f(type(), type()) -> ok.
f(Ty1 = {type, _, tuple, Tys1}, Ty2 = {type, _, tuple, Tys2}) ->
    case {Tys1, Tys2} of
        {any, _} -> ok;
        _ -> ok
    end.
