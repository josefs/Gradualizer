%% convenience guards

%% same as typechecker:is_int_type/2 but can be used as a guard
-define(is_int_type(T),
        (tuple_size(T) =:= 4 andalso
         element(1, T) =:= type andalso
         (element(3, T) =:= integer orelse
          element(3, T) =:= pos_integer orelse
          element(3, T) =:= neg_integer orelse
          element(3, T) =:= non_neg_integer orelse
          element(3, T) =:= range))
        orelse
          (tuple_size(T) =:= 3 andalso
           element(1, T) =:= integer)).
