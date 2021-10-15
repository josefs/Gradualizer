-module(absform_tests).

-include_lib("eunit/include/eunit.hrl").
-include("typechecker.hrl").

function_type_list_to_fun_types_test() ->
    {attribute, _, spec, {_, FunTypeList}} =
        merl:quote("-spec f(T)-> boolean() when T :: tuple();"
                   "       (atom()) -> any()."),
    BoundedFunTypeList = absform:normalize_function_type_list(FunTypeList),
    Ty = typechecker:bounded_type_list_to_type(
           #env{tenv = gradualizer_lib:create_tenv(?MODULE, [], [])}, BoundedFunTypeList),
    ?assertMatch({type, 0, union,
                  [{type,1,'fun',
                    [{type,1,product,[{type,0,tuple,any}]},
                     {type,1,boolean,[]}]},
                   {type,1,'fun',
                    [{type,1,product,[{type,1,atom,[]}]},
                     {type,1,any,[]}]}]}, Ty),
    ok.
