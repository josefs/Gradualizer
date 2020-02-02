-module(user_default).

-export([c/1]).

c(File) ->
    % TODO: we can be a lot more clever about recognizing
    % if the argument is a file or a module, just like
    % shell_default:c/1.
    case gradualizer:type_check_file(File) of
        ok ->
            shell_default:c(File);
        Err ->
            Err
    end.
