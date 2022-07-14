-module(messaging_pass).

-export([main/0, server/0, server1/0, server2/0, server3/0, server4/0]).

-spec ok() -> ok.
ok() -> ok.

server() ->
  receive
    quit ->
      io:format("Quitting!~n");
    Foo ->
      io:format("~p~n", [Foo]),
      server()
  end.

-spec server1() -> ok.
server1() ->
  receive
    quit ->
      ok();
    Foo ->
      io:format("~p~n", [Foo]),
      server1()
  end.

-spec server2() -> ok.
server2() ->
  A = receive
    quit ->
      ok();
    Foo ->
      io:format("~p~n", [Foo]),
      server2()
  end,
  A.

-spec server3() -> ok.
server3() ->
  receive
    quit ->
      ok();
    Foo ->
      io:format("~p~n", [Foo]),
      server3()
  after 1000 -> ok
  end.

-spec server4() -> ok.
server4() ->
  A = receive
    quit ->
      ok();
    Foo ->
      io:format("~p~n", [Foo]),
      server4()
    after 1000 -> ok()
  end,
  A.

main() ->
  Pid = spawn(fun server/0),
  Pid!{apa,bepa},
  Pid!quit.
