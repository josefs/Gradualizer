-module(filename).

-type deep_list() :: lists:deep_list(char() | atom()).

-spec basename(binary()) -> binary();
              (string() | atom() | deep_list()) -> string().

-spec dirname(binary()) -> binary();
             (string() | atom() | deep_list()) -> string().

-spec rootname(binary()) -> binary();
              (string() | atom() | deep_list()) -> string().

-type name() :: string() | atom() | deep_list().
-type bname() :: string() | atom() | deep_list() | binary().

-spec join([name()]) -> string();
          ([bname()]) -> binary().

-spec join(name(),  name())  -> string();
          (bname(), name())  -> binary();
          (name(),  bname()) -> binary();
          (bname(), bname()) -> binary().
