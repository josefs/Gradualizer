-module(absform_tests).

-include_lib("eunit/include/eunit.hrl").
-include("typechecker.hrl").

function_type_list_to_fun_types_test() ->
    {attribute, _, spec, {_, FunTypeList}} =
        merl:quote("-spec f(T)-> boolean() when T :: tuple();"
                   "       (atom()) -> any()."),
    FunTypeListNoPos = lists:map(fun typelib:remove_pos/1, FunTypeList),
    BoundedFunTypeList = absform:normalize_function_type_list(FunTypeListNoPos),
    Env = test_lib:create_env([]),
    Ty = typechecker:bounded_type_list_to_type(Env, BoundedFunTypeList),
    ?assertMatch({type, 0, union,
                  [{type,0,'fun',
                    [{type,0,product,[{type,0,tuple,any}]},
                     {type,0,boolean,[]}]},
                   {type,0,'fun',
                    [{type,0,product,[{type,0,atom,[]}]},
                     {type,0,any,[]}]}]}, Ty),
    ok.
