-module(filename).

-type deep_list() :: lists:deep_list(char() | atom()).

-spec basename(binary()) -> binary();
              (string() | atom() | deep_list()) -> string().

-spec rootname(binary()) -> binary();
              (string() | atom() | deep_list()) -> string().

-spec join([string() | atom() | deep_list()]) -> string();
          ([string() | atom() | deep_list() | binary()]) -> binary().
