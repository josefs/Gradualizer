-define(BUILT_IN_SPECS, #{
    {erlang, '+', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]},
         {type,0,'fun',[{type,0,product,[{type,0,float,[]},{type,0,integer,[]}]},{type,0,float,[]}]},
         {type,0,'fun',[{type,0,product,[{type,0,integer,[]},{type,0,float,[]}]},{type,0,float,[]}]},
         {type,0,'fun',[{type,0,product,[{type,0,float,[]},{type,0,float,[]}]},{type,0,float,[]}]}],
    {erlang, '-', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]},
         {type,0,'fun',[{type,0,product,[{type,0,float,[]},{type,0,integer,[]}]},{type,0,float,[]}]},
         {type,0,'fun',[{type,0,product,[{type,0,integer,[]},{type,0,float,[]}]},{type,0,float,[]}]},
         {type,0,'fun',[{type,0,product,[{type,0,float,[]},{type,0,float,[]}]},{type,0,float,[]}]}],
    {erlang, '*', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]},
         {type,0,'fun',[{type,0,product,[{type,0,float,[]},{type,0,integer,[]}]},{type,0,float,[]}]},
         {type,0,'fun',[{type,0,product,[{type,0,integer,[]},{type,0,float,[]}]},{type,0,float,[]}]},
         {type,0,'fun',[{type,0,product,[{type,0,float,[]},{type,0,float,[]}]},{type,0,float,[]}]}],
    {erlang, '/', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,number,[]},{type,0,number,[]}]},{type,0,float,[]}]}],
    {erlang, 'div', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]}],
    {erlang, 'rem', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]}],
    {erlang, 'band', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]}],
    {erlang, 'bor', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]}],
    {erlang, 'bxor', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]}],
    {erlang, 'bsl', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]}],
    {erlang, 'bsr', 2} =>
        [{type,0,'fun',[{type,0,product, [{type,0,integer,[]},{type,0,integer,[]}]},{type,0,integer,[]}]}]
}).
