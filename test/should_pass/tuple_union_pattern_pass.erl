-module(tuple_union_pattern_pass).

-export([f/1]).

-type type() :: af_annotated_type() | af_fun_type().

-type anno() :: erl_anno:anno().

-type af_anno() :: af_variable().

-type af_variable() :: {'var', anno(), atom()}. % | af_anon_variable()

-type af_annotated_type() ::
        {'ann_type', anno(), [af_anno() | type()]}. % [Var, Type]

-type af_fun_type() :: {'type', anno(), 'fun', []}
                     | {'type', anno(), 'fun', [{'type', anno(), 'any'} | type()]}.

-spec f(type()) -> ok.
f({type, _L, 'fun', [_Any = {type, _, any}, _RetTy]}) -> ok.
