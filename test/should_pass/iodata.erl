-module(iodata).

-export([g/0]).

-spec g() -> term().
g() ->
    expect_iodata("foo"),
    expect_iodata(["foo"]),
    expect_iodata(<<"foo">>).

-spec expect_iodata(iodata()) -> any().
expect_iodata(_IOData) ->
    ok.
