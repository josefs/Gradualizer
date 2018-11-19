-module(type_refinement).

-export([basic_type_refinement/1]).

%% Test that Value is not considered to be string() | false.
-spec basic_type_refinement(string()) -> string().
basic_type_refinement(Key) ->
    case getenv(Key) of
        false -> "banana";
        Value -> Value
    end.


%% A simplified version of os:getenv/1.
-spec getenv(Key :: string()) -> Value :: string() | false.
getenv("SHELL") -> "/bin/sh";
getenv("USER")  -> "joe";
getenv(_)       -> false.
