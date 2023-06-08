These are some experiments with the disjunctive normal form we mentioned
in https://github.com/josefs/Gradualizer/pull/524#discussion_r1200493035.

First, https://typex.fly.dev/ seems to actually call CDuce under the hood.
This can be inferred from the following error message generated based on some invalid input.
Input:

```
g :: (_ -> {:t, :a} | {:t, :b})
def g(_), do: {:t, :b}

tuple_case :: (_ -> {:t, :a or :b})
def tuple_case(arg), do: g(arg)

tuple_case(:z)
```

Message:

```
Typing error:
After extracting domain types from
	{ _fun =[Any] -> |({ _tup=[`t `a] ; _size=2 }, { _tup=[`t `b] ; _size=2 }) ; _ari = 1 }
Failed to parse the result:
	Cduce_core.Parser.MenhirBasics.Error  <-------------- Cduce error
```

Second, let's consider:

```erlang
-spec g() -> {t, a | b}.
g() -> {t, a}.

-spec tuple_case1() -> {t, a} | {t, b}.
tuple_case1() ->
    R = g(),
    R.

-spec tuple_case2() -> {t, a} | {t, b}.
tuple_case2() ->
    g().
```

The following equivalent examples were tested with https://typex.fly.dev/:

```
g :: (_ -> {:t, :a or :b})
def g(_), do: {:t, :b}

tuple_case :: (_ -> {:t, :a} or {:t, :b})
def tuple_case(arg), do: g(arg)

tuple_case(:z)
```

```
g :: (_ -> {:t, :a} or {:t, :b})
def g(_), do: {:t, :b}

tuple_case :: (_ -> {:t, :a or :b})
def tuple_case(arg), do: g(arg)

tuple_case(:z)
```

They both typecheck.
This means that the Elixir prototype based on CDuce does indeed
treat {t, a | b} :: {t, a} | {t, b}
as well as {t, a} | {t, b} :: {t, a | b}.

This can be achieved to some extent in Gradualizer, too,
but it comes with significant breakage of tests.
