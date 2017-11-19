# Principles for the type system

* The type system will be based on the established theory of gradual types.

* Without any annotations there should be no type errors. There will be no
  type inference.

* We want to support as much of the already existing type system as possible.
  In some cases we will want to improve on dialyzer such that we report
  more errors, while at other times we might report fewer errors.

  * Syntactically we will support all existing type annotations.

* In general, the new type system should work well in existing code bases
  with lots of type annotations.

* Process communication will be untyped. Given erlangs open world assumption
  when communicating and the use of a mailbox it is almost impossible to give
  sensible types.

  In certain special cases it may be possible to use session types to
  give types to the communication, but that would almost certainly
  require some kind of abstraction on top of the mailbox in order to make
  it manageable.

* No blame calculus in the first iteration of the type system.

# Notes on how to represent types

* The type any() should NOT be the same as untyped. The type any() is the
  supertype of all other types, but it is not a subtype of any type. This
  subtyping relationship needs to hold in our type system as well. Hence
  we need a separate type which is untyped.

* How should I deal with improper lists? They're kind of a supertype of
  ordinary lists so maybe I can deal with them using subtyping.

# How to process

It getting harder and harder to avoid formalizing some kind of type
system for erlang. Core erlang is an obvious candidate to start
with. Though I will have to add types to it. The implementation of
core erlang has a field for annotations and I suppose I could use that
to insert type information in the right places.

I should start small. The type system fragment I should start with should be
roughly the STLC. Then I can work my way up from there.

Eventually I'd like to support plt files. That means that gradualizer should
be able to read plt files written by dialyzer. However, that might cause a
problem. If dialyzer infers types for untyped functions then gradualizer might
report errors even though it shouldn't. It depends on whether it is possible
to determine from the plt file whether the type was present in the source
file or inferred.

# Future work

* Cross-process blame. It'd be interesting to develop a blame calculus that
  can be used as a compilation target on top of erlang. Blame can be sent
  as messages. That will go agains the open world assumption and requires
  that all nodes be compiled via the blame calculus.

  This will be an interesting alternative to session types.

  Thanks to Alejandro for suggestion I think about this.

* Ultimately it would be nice to change the compilation of erlang to
  take advantage of the type information. That is a long way away
  though, especially since it is an unsolved problem how to compile casts in
  the blame calculus efficiently.
