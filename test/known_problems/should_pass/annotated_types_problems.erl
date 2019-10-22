-module(annotated_types_problems).

-export([named_record_arg/1]).

-record(r, {}).
-type r() :: #r{}.

-spec named_record_arg(R :: r()) -> {ok, R}.
named_record_arg(#r{} = R) -> {ok, R}.
