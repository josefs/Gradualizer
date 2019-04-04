-module(glb_ann_types).

-compile(export_all).

-type type() :: erl_parse:abstract_type().

-define(TYPE_MOD, erl_parse).
%%-define(TYPE_MOD, gradualizer_type).

-spec type_check_comprehension_in(
        Expr       :: erl_parse:abstract_expr(),
        Qualifiers :: [ListGen |Filter|BinGen]) ->
     {map(), constraints:constraints()}
       when
        ListGen :: {generate, erl_anno:anno(), ?TYPE_MOD:abstract_expr(), ?TYPE_MOD:abstract_expr()},
        BinGen  :: {b_generate, erl_anno:anno(), ?TYPE_MOD:abstract_expr(), ?TYPE_MOD:abstract_expr()},
        Filter  :: ?TYPE_MOD:abstract_expr().
type_check_comprehension_in(Expr, [{generate, P_Gen, Pat, Gen} | Quals]) ->
    {#{}, constraints:empty()}.

