---
title:  A Gradual Typesystem for Erlang
author: Josef Svenningsson
abstract: |
  This is the abstract.

  It consists of two paragraphs.
...


## A Type System for Erlang

::: incremental

* I've developed a new type system for Erlang

* Many people before me have tried and failed

* What gives me the hubris to think I can succeed?

* Gradual typing!

:::

# Gradual Typing Background

## The Static vs Dynamic Typing War

Should a language have **Static** or **Dynamic** Typing?

. . .

As Erlang programmers we all know the advantages of Dynamic Typic

* Programming becomes more agile

  It's very quick to write and immediately run the program

* It's the right choice for Erlang-style message passing

* Easily handles situations where the type of a value depends on runtime
  information



## Static Typing advantages

* Catches bugs earlier

* Type signatures provide documentation and help understanding

* Types can help structuring code

## Soundness for Static Typing


> Well typed programs can't go wrong

. . .

* **Sound** type systems can guarantee the absence of whole classes of errors

## Static vs Dynamic

* Dynamic typing is particularly sweet for small, script-like, programs.

* Static typing becomes increasing helpful as programs grow large

## Gradual Typing

**Gradual Typing** is a method for **mixing** static and dynamic typing.


* Start with a dynamically program and **gradually** add type annotations
  to make the program more statically typed.

* Start out with a dynamically typed small program.

  As the program grow larger, add type signatures to make it more
  statically typed.

## Soundness for Gradual Typing

Instead of **soundness**, gradual typing has **gradual guarantee**:

> If the program fails, the problem lies in the dynamically typed part

. . .

The new slogan is:

> Well typed programs can't be blamed


## Examples of Languages with Gradual Typing

Gradual typing has been a hot research topic over the last decade

* Javascript
  * Type Script
  * Clojure
  * ActionScript
* PHP
  * Hack
* Python
  * Reticulated Python
* Racket
  * Typed Racket
* C#
* ...

# Gradual typing primer

## Gradual typing primer

An ordinary type system + a type for dynamic typing

Two choices for default typing:

* The default is static typing
  * The programmer has to annotate dynamically typed parts explicitly
* The default is dynamic typing
  * The programmer has to add static types explicitly

## Compatibility

In a standard static type system, types are compared to **equality**
or **subtyping**.

* Equality

  `integer() == integer()`, `integer() /= boolean()`

* Subtyping

  `integer() :: number()`




# Gradual Typing in Erlang

## Principles for the Type System

The programmer is in control.

Type specs are the means of control.

No type specs = no type checking

## Gradual Typing in Erlang

* Type any() means dynamic type.

* Subtyping

  * term() is the top of the subtyping hierarchy
  * none() is the bottom of the hierarchy

## Principles

* The type system is backwards compatible with all existing type specs,
  record declarations and type defintions.

=> Works well on existing code bases

## Process communication

* Process communication is always dynamically typed

* Rationale:

  * Types don't fit very well with Erlang-style message passing with an
	open-world assumption.

# Examples

## Example

TODO: Example of how to add types to a program gradually to catch errors

# Type annotations

## Type annotation
* Currently, type specs can only be given on the toplevel on functions.

* This is enough for most circumstances.

* But sometimes it would be nice to have type annotations on expressions

## Possible type annotation extension

It's tempting to extend Erlang with type annotations on expressions

* A new grammatical form:

  E :: T

* However, we have a more lightweight solution for this problem.

## A library of type annotation functions

We provide a small library of functions which can act as type annotations

TODO: Examples

# Current status of Gradualizer

## Status

* Alpha-quality. A beta-release soonish

* Not much used in practice yet.

* Please try it out! But beware of bugs!

  https://github.com/josefs/Gradualizer

# Comparison With Dialyzer

## Dialyzer

Do we really need the Gradualizer?

We already have the Dialyzer!

. . .

* The Dialyzer is *not* a type system

* It's a *discrepancy analyzer*, which tried to predict runtime errors

* It tries it best to avoid false negatives

* It doesn't use type specs to guide the analysis,
  it infers information and then checks against the type specs.

## Concrete differences between Gradualizer and Dialyzer

What follows is a collection of examples to show the difference between
Gradualizer and Dialyzer.

The examples may look contrived, but most of them come from real code.

## Restrictions on the Nesting Depth of Types

Dialyzer will only go so far when trying to detect errors in nested types.

``` {.erlang}
-spec depth4() -> {1,{2,{3,4}}}.
depth4() -> {1,{2,{3,5}}}.

-spec depth5() -> {1,{2,{3,{4,5}}}}.
depth5() -> {1,{2,{3,{4,6}}}}.

```

* Dialyzer will detect the error in `depth4/0` but not `depth5/0`
* Gradualyzer will detect both

. . .

* In fareness, this is rarely a problem in practice

## Return type error

``` {.erlang}
-spec pattern_test(integer()) -> {}.
pattern_test(1) ->
    true;
pattern_test(X) ->
    {}.
```

* Dialyzer will not detect the error in the above program
  * Using the flag `-Woverspecs` will detect it though.
* Gradualizer detects the error

## Unions

``` {.erlang}
-spec tuple_union() -> {undefined, binary()}
                     | {integer(), undefined}.
tuple_union() ->
    {undefined, undefined}.
```

* Dialyzer approximates a union of tuples as a tuple of unions.

  In this case it means that it approximates the return type as
  `{undefined | integer(), binary() | undefined}`

  The result is that Dialyzer fails to catch the error
  
* Gradualizer detects the error

## Unions - 2

A modification of the example, with pattern matching on the union.

``` {.erlang}
-spec tuple_union({undefined, {}}
                | {{}, undefined}) -> {}.
tuple_union({undefined, undefined}) ->
    {};
tuple_union({{},{}}) ->
    {}.
```

* Both Dialyzer and Gradualizer fails to catch this problem

## Flow Sensitivity

``` {.erlang}
-export([foo/0]).

foo() ->
	bar(apa).

-spec :: (apa | bepa) -> true | false.
bar(apa) ->
	true;
bar(bepa) ->
	false.
```

* Dialyzer can figure out that the clause `bar(bepa)`
  will never be called.

* With `-Wunderspecs`, Dialyzer will also complain that `bar` will never
  return `false`.

* Gradualizer cannot do any of that.

## Speed

Gradualizer to typically **alot** faster than Dialyzer

TODO: More here.

# Future work

## Future work

* Type apply/3 so that certain atoms can be typed as behaviours.
  We can then check if the called function actually exists.

* Type annotations on expressions

* Soundness proof

* Currently, all types are erased before compilation.

  It is possible to optimize compilation based on the type information.

## Acknowledgements

Thanks to

* Viktor Söderqvist for hacking on the tool and general discussions
* David Wahlstedt for discussions on type systems and general support

##

\begin{center}

Thank you!

\url{http://github.com/josefs/Gradualizer}

\end{center}
