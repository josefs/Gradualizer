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

# How to proceed

It getting harder and harder to avoid formalizing some kind of type
system for erlang. Core erlang is an obvious candidate to start
with. Though I will have to add types to it. The implementation of
core erlang has a field for annotations and I suppose I could use that
to insert type information in the right places.

I should start small. The type system fragment I should start with should be
roughly the STLC. Then I can work my way up from there. It seems to me that
"Abstracting Gradual Types" is a good model for how to incorporate new 
type system features.

Eventually I'd like to support plt files. That means that gradualizer should
be able to read plt files written by dialyzer. However, that might cause a
problem. If dialyzer infers types for untyped functions then gradualizer might
report errors even though it shouldn't. It depends on whether it is possible
to determine from the plt file whether the type was present in the source
file or inferred.

# TODO

* [ ] Formalize the properties of gradual type systems as QuickCheck/Proper
      properties. This will need good generators of Erlang programs.

      The properties of gradual type systems are listed in the paper
      "Refined Criteria for Gradual Typing":

      http://drops.dagstuhl.de/opus/volltexte/2015/5031/pdf/21.pdf

      The following paper shows how to generate well-typed terms:

      http://users.eecs.northwestern.edu/~baf111/random-judgments/random-judgments-esop15.pdf

* [ ] Support for plt-files.

      This is going to be a bit harder than I expected. The types in
      plt-files uses a very different representation. Also, there
      doesn't seem to be any way of extracting all types from a
      plt-file.

* [ ] Better error messages.

* [ ] Warn about unsupported types.
      Unsupported types should be converted to any().

* [ ] Make it a standalone program.

* [ ] Test suite

* [ ] Extend support for all of Erlang.

  * [x] Message passing constructs
  * [ ] Exception constructs
  * [ ] Guards
  * [ ] Binary syntax
  * [ ] Records
  * [ ] List comprehensions
  * [ ] If expressions
  * [ ] Maps
  * [ ] An initial environment for the erlang module

# Future work

* Cross-process blame. It'd be interesting to develop a blame calculus that
  can be used as a compilation target on top of erlang. Blame can be sent
  as messages. That will go agains the open world assumption and requires
  that all nodes be compiled via the blame calculus.

  This will be an interesting alternative to session types.

  Thanks to Alejandro for suggestion I think about this.

* The existing type system for Erlang supports equi-recursive types. In order
  to deal with them properly I should look into using the algorithms from:
  "On Subtyping-Relation Completeness, with an Application to Iso-Recursive Types"

* Since the existing type system supports union types and a restricted form
  of intersection types, the following paper seems to provide the way to deal
  with integrating these features in a gradual type system:
  "Gradual Typing with Union and Intersection Types"
  
  http://www.dptinfo.ens-cachan.fr/~vlanvin/Papers/icfp17.pdf
  
* Ultimately it would be nice to change the compilation of erlang to
  take advantage of the type information. That is a long way away
  though, especially since it is an unsolved problem how to compile casts in
  the blame calculus efficiently.
