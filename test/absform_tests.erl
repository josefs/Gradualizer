-module(absform_tests).

-include_lib("eunit/include/eunit.hrl").

function_type_list_to_fun_types_test() ->
    {attribute, _, spec, {_, FunTypeList}} =
        merl:quote("-spec f(T)-> boolean() when T :: tuple();"
                   "       (atom()) -> any()."),
    BoundedFunTypeList = absform:normalize_function_type_list(FunTypeList),
    {Ty, Cs} = absform:function_type_list_to_fun_types(BoundedFunTypeList),
    ?assertMatch({type, 1, union,
                  [{type,1,'fun',
                    [{type,1,product,[{var,1,'T'}]},
                     {type,1,boolean,[]}]},
                   {type,1,'fun',
                    [{type,1,product,[{type,1,atom,[]}]},
                     {type,1,any,[]}]}]}, Ty),
    ExpCs = constraints:upper('T', typelib:parse_type("tuple()")),
    ?assertMatch(ExpCs, Cs),
    ok.
