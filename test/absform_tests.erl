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
    ?assertMatch({type,0,'fun',
                  [
                    {type,0,product,[
                      {type, 0, union, [{type, 0, atom, []}, {type, 0, tuple, any}]}
                    ]},
                    {type, 0, union, [
                      {type, 0, any, []},
                      {type, 0, union, [{atom, 0, false}, {atom, 0, true}]}
                    ]}
                  ]}, Ty),
    ok.

extract_function_from_call_test() ->
  CallExpr = merl:quote("add(5, Pi)"),
  FunExpr = merl:quote("fun add/2"),
  {'fun', _, ExpectedFunction} = FunExpr,
  Function = absform:extract_function_from_call(CallExpr),
  ?assertEqual(ExpectedFunction, Function).

extract_function_from_call_remote_test() ->
  CallExpr = merl:quote("nums:add(5, Pi)"),
  FunExpr = merl:quote("fun nums:add/2"),
  {'fun', _, ExpectedFunction} = FunExpr,
  Function = absform:extract_function_from_call(CallExpr),
  ?assertEqual(ExpectedFunction, Function).
