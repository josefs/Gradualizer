-module(messaging_fail).

-export([server1/0, server2/0, server3/0, server4/0]).

-spec ok() -> ok.
ok() -> ok.

-spec nok() -> nok.
nok() -> nok.

-spec server1() -> nok.
server1() ->
  receive
    quit ->
      ok();
    Foo ->
      io:format("~p~n", [Foo]),
      server1()
  end.

-spec server2() -> nok.
server2() ->
  A = receive
    quit ->
      ok();
    Foo ->
      io:format("~p~n", [Foo]),
      server2()
  end,
  A.

-spec server3() -> nok.
server3() ->
  receive
    quit ->
      nok();
    Foo ->
      io:format("~p~n", [Foo]),
      server3()
  after 1000 -> ok()
  end.

-spec server4() -> nok.
server4() ->
  A = receive
    quit ->
      nok();
    Foo ->
      io:format("~p~n", [Foo]),
      server4()
    after 1000 -> ok()
  end,
  A.
