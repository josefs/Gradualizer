-module(pattern_record).

-record(test, {
    field :: integer()
}).

-spec clause_match(term()) -> #test{} | not_test.
clause_match(#test{} = Test) -> Test;
clause_match(_) -> not_test.

-spec clause_match2(term()) -> #test{} | not_test.
clause_match2(Test = #test{}) -> Test;
clause_match2(_) -> not_test.

-spec pat_match(term()) -> #test{} | not_test.
pat_match(T) ->
    case T of
        #test{} = Test -> Test;
        _ -> not_test
    end.

-spec pat_match2(term()) -> #test{} | not_test.
pat_match2(T) ->
    case T of
        Test = #test{} -> Test;
        _ -> not_test
    end.

-spec any(any()) -> #test{} | not_test.
any(#test{} = Test) -> Test;
any(_) -> not_test.

-spec any(any()) -> #test{field :: 42} | not_test.
any(#test{} = Test) -> Test;
any(_) -> not_test.

-spec test42(#test{field :: 42} | bananas) -> #test{field :: 42} | not_test.
test42(#test{} = Test) -> Test;
test42(_) -> not_test.
