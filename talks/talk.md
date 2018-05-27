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

Both choices have their advantages


| Static                     | Dynamic                                 |
|----------------------------|-----------------------------------------|
| Helps structuring code     | Good for open-world message passing     |


## Gradual Typing

A method for combining **static typing** and **dynamic typing**.

Hot research topic over the last decade

Examples of implementations of gradual types:

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


# Gradual Typing in Erlang

## Principles for the Type System

The programmer is in control.

Type specs are the means of control.

No type specs = no type checking

## Gradual Typing in Erlang

* Type type any() means dynamic type.

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

## Current status of Gradualizer

* Alpha-quality. A beta-release soonish

* Not much used in practice yet.

* Please try it out! But beware of bugs!

# Comparison With Dialyzer

## 

Do we really need the Gradualizer?

We already have the Dialyzer!

## Dialyzer

* The Dialyzer is *not* a type system

* It's a *discrepancy analyzer*, which tried to predict runtime errors

* It tries it best to avoid false negatives

## Concrete differences between Gradualizer and Dialyzer

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
  * Using the flag `-Wunderspec` will detect it though.
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
