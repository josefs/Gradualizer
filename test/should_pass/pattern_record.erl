-module(pattern_record).

-compile(export_all).

-record(test, {
    field :: integer()
}).

-spec clause_match(#test{} | whatever) -> #test{} | not_test.
clause_match(#test{} = Test) -> Test;
clause_match(_) -> not_test.

-spec clause_match2(#test{} | whatever) -> #test{} | not_test.
clause_match2(Test = #test{}) -> Test;
clause_match2(_) -> not_test.

-spec pat_match(#test{} | whatever) -> #test{} | not_test.
pat_match(T) ->
    case T of
        #test{} = Test -> Test;
        _ -> not_test
    end.

-spec pat_match2(#test{} | whatever) -> #test{} | not_test.
pat_match2(T) ->
    case T of
        Test = #test{} -> Test;
        _ -> not_test
    end.

-record(r1, {
    f :: integer()
}).

-record(r2, {
    f :: atom()
}).

-spec pass(#r1{} | #r2{}) -> integer().
good(R = #r1{f = F}) -> R#r1.f + F;
good(_) -> 0.

-record(r3, {
    r :: #r1{} | #r2{}
}).

-spec multiple(#r3{} | #r2{}) -> integer().
multiple(#r3{r = R = #r1{}}) -> R#r1.f;
multiple(_) -> 0.
