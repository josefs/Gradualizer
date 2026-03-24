-module(sigils_and_doc_pass).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 27).

-compile([export_all, nowarn_export_all]).

%% -doc attribute (OTP 27+)
-doc "Returns a greeting binary using a sigil.".
-spec sigil_binary() -> binary().
sigil_binary() ->
    ~"hello world".

-doc "Returns a greeting string using the ~s sigil.".
-spec sigil_string() -> string().
sigil_string() ->
    ~s"hello world".

-doc #{since => "1.0"}.
-spec sigil_verbatim_binary() -> binary().
sigil_verbatim_binary() ->
    ~B"hello world".

%% Sigil binary in a match
-spec sigil_match() -> binary().
sigil_match() ->
    X = ~"test",
    X.

%% Sigil binary concatenation
-spec sigil_concat() -> binary().
sigil_concat() ->
    <<(~"hello")/binary, " ", (~"world")/binary>>.

%% Sigil in a function argument
-spec sigil_arg() -> non_neg_integer().
sigil_arg() ->
    byte_size(~"hello").

-endif. %% OTP >= 27
-endif. %% OTP_RELEASE
