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
