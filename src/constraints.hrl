-ifndef(__CONSTRAINTS_HRL__).
-define(__CONSTRAINTS_HRL__, true).

-record(constraints, { lower_bounds = #{} :: #{ constraints:var() =>
                                                gradualizer_type:abstract_type() },
                           upper_bounds = #{} :: #{ constraints:var() =>
                                                gradualizer_type:abstract_type() } }).

-endif. %% __CONSTRAINTS_HRL__
