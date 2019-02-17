-module(iodata_fail).

-export([utf/0]).

%% an iodata can only contain bytes, no utf characters
utf() ->
    %% the last character is hexadecimal 256
    %% (unicode "Latin Capital Letter a with Macron")
    expect_iodata("foo\x{100}").

-spec expect_iodata(iodata()) -> any().
expect_iodata(_IOData) ->
    ok.
