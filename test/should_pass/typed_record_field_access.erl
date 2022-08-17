-module(typed_record_field_access).

-record(r, {a, b}).

-type r() :: #r{a :: integer()}.

-spec b(r()) -> any().
b(R) ->
    R#r.b.
