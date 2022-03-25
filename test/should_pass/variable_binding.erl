-module(variable_binding).

-export([variable_bind_before_record_pattern_match/2]).

-record(st, {}).

variable_bind_before_record_pattern_match({Pid}, #st{}) ->
    Pid.
