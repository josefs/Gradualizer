-module(gradualizer_prelude_parse_trans).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    [{attribute, _, file, _} = File,
     {attribute, _, module, _} = Mod
     |SpecForms] = Forms,

    [File,
     Mod,
     {attribute,2,export,[{get_prelude,0}]},
     {function,3,get_prelude,0,
      [{clause,3,[],[],
        [erl_parse:abstract(SpecForms)]}]}].
