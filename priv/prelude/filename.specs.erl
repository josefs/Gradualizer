-module(filename).

-type deep_list() :: lists:deep_list(char() | atom()).

-spec basename(binary()) -> binary();
              (string() | atom() | deep_list()) -> string().

-spec dirname(binary()) -> binary();
             (string() | atom() | deep_list()) -> string().

-spec rootname(binary()) -> binary();
              (string() | atom() | deep_list()) -> string().

-type name() :: string() | atom() | deep_list().

-spec join([name()]) -> string();
          ([binary()]) -> binary().

-spec join(name(), name()) -> string();
          (binary(), name()) -> binary();
          (name(), binary()) -> binary();
          (binary(), binary()) -> binary().
