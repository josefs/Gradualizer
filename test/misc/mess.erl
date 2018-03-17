-module(mess).

-compile([export_all]).

server() ->
  receive
    quit ->
      io:format("Quitting!~n");
    Foo ->
      io:format("~p~n", [Foo]),
      server()
  end.

main() ->
  Pid = spawn(fun server/0),
  Pid!{apa,bepa},
  Pid!quit.
