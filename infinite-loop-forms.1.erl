[{attribute,0,module,module_name},
 {attribute,0,record,
  {r1,[{record_field,0,{atom,0,f1}},{record_field,0,{atom,0,f2}}]}},
 {attribute,0,record,{r2,[]}},
 {attribute,0,record,{r3,[{record_field,0,{atom,0,f1}}]}},
 {attribute,0,export,[{f1,1},{f1,2},{f2,0}]},
 {function,0,f1,1,
  [{clause,0,
    [{record,0,r1,[{record_field,0,{atom,0,f2},{integer,0,19}}]}],
    [],
    [{float,0,24.819461186751518}]}]},
 {function,0,f1,2,
  [{clause,0,
    [{atom,0,false},{map,0,[{map_field_exact,0,{atom,0,true},{atom,0,true}}]}],
    [[{string,0,"sUa"},
      {op,0,'=<',{string,0,"UiXn"},{op,0,'rem',{nil,0},{integer,0,13}}}]],
    [{char,0,119},{string,0,[]}]}]},
 {function,0,f2,0,
  [{clause,0,[],[],
    [{block,0,
      [{lc,0,
        {string,0,[]},
        [{nil,0},
         {b_generate,0,
          {bin,0,
           [{bin_element,0,{integer,0,16},{nil,0},[{unit,16}]},
            {bin_element,0,{float,0,34.23178094575535},{char,0,114},[bits]}]},
          {bin,0,
           [{bin_element,0,{float,0,110.15487133390829},default,[big,utf16]},
            {bin_element,0,
             {'case',0,
              {integer,0,26},
              [{clause,0,[{var,0,'V0'}],[],[{char,0,105}]},
               {clause,0,
                [{integer,0,0}],
                [[{nil,0}],[{string,0,"a"}]],
                [{'if',0,
                  [{clause,0,[],[],[{map,0,{integer,0,28},[]}]},
                   {clause,0,[],[],
                    [{tuple,0,[{integer,0,0},{string,0,"md"}]}]}]}]}]},
             {'receive',0,
              [{clause,0,
                [{map,0,[{map_field_exact,0,{atom,0,true},{atom,0,false}}]}],
                [[{integer,0,11}]],
                [{nil,0}]}]},
             [little]}]}}]}]},
     {char,0,113}]}]}].
