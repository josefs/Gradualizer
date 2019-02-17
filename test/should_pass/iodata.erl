-module(iodata).

-export([f/0, g/0]).

-spec f() -> iolist().
f() ->
    [$a|list_to_binary("b")].

-spec g() -> term().
g() ->
    expect_iodata("foo"),
    expect_iodata(["foo"]),
    expect_iodata(<<"foo">>).

-spec expect_iodata(iodata()) -> any().
expect_iodata(_IOData) ->
    ok.
