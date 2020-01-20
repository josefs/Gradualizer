-module(gradualizer_int_tests).

-include_lib("eunit/include/eunit.hrl").

int_range_diff_test_() ->
    [
     ?_assertEqual([{1, 2}, {6, 10}],
                   gradualizer_int:int_range_diff({1, 10}, {3, 5})),
     ?_assertEqual([{neg_inf, -11}],
                   gradualizer_int:int_range_diff({neg_inf, -1}, {-10, pos_inf})),
     ?_assertEqual([{neg_inf, -11}],
                   gradualizer_int:int_range_diff({neg_inf, pos_inf}, {-10, pos_inf})),
     ?_assertEqual([{10, pos_inf}],
                   gradualizer_int:int_range_diff({neg_inf, pos_inf}, {neg_inf, 9})),
     ?_assertEqual([{10, pos_inf}],
                   gradualizer_int:int_range_diff({0, pos_inf}, {neg_inf, 9})),
     ?_assertEqual([],
                   gradualizer_int:int_range_diff({1, 10}, {neg_inf, pos_inf})),
     ?_assertEqual([{1, 10}],
                   gradualizer_int:int_range_diff({1, 10}, {-10, -1})),
     ?_assertEqual([{2, 2}],
                   gradualizer_int:int_range_diff({1, 2}, {1, 1})),
     ?_assertEqual([{1, 1}],
                   gradualizer_int:int_range_diff({1, 2}, {2, 2}))
    ].
