-module(filename).

-type deep_list() :: lists:deep_list(char() | atom()).

-spec rootname(binary()) -> binary();
              (string() | atom() | deep_list()) -> string().
