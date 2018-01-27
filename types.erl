-module(types).

% Every subterm of a type has an identity. 
-type(type(), {int(), type_rec()}). 

-type(type_rec(), int() | ty_var() | tuple(list(type())) ). 

process_type(Type) ->
  {Type, warning}. 
