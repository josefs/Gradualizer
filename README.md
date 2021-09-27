<h1 align="center">Gradualizer: A Gradual Type System for Erlang</h1>
<h2 align="center">A type checker for Erlang</h2>
<p align="center">
  <a href="https://github.com/josefs/Gradualizer/actions/workflows/build-and-test.yml">
    <img src="https://github.com/josefs/Gradualizer/actions/workflows/build-and-test.yml/badge.svg"
         alt="GitHub Actions Build and Test Status" />
  </a>
</p>

# The idea

Gradualizer aims to integrate well into existing Erlang code bases in a non intrusive way. It does so by

* having a type system that is based on the principles of Gradual Typing
* using the existing type specs syntax in Erlang.
* allowing for granular opt-in of type checking. Without any type specs, no static typing happens.

  When type specs are added the program is checked against
  these specs statically. The more type specs, the more static typing.

# Usage

## To run Gradualizer from the command line

Compile the project as an escript. Then use it to check beam files or erl
files. Use the `-h` option for help.

    make escript

    bin/gradualizer [ OPTIONS ] [ FILES TO CHECK ]

## To run Gradualizer from the Erlang shell

Launch the interactive prompt with all the relevant modules in the path. Then,
use the functions in the `gradualizer` module.

    make shell

    1> gradualizer:type_check_file("path/to/some_file.erl").

## To run Gradualizer from rebar3 or Mix

There is a rebar3 plugin included. See [examples/rebar3/README.md](examples/rebar3/README.md).

For Mix, a wrapper is provided at https://github.com/OvermindDL1/gradualixir.

## Prerequisites

The Gradualizer requires at least OTP 21 and is built using plain OTP
functionality and a self-contained Makefile; alternetively using
[rebar3](https://www.rebar3.org/). It can also be built using Mix if
used as a dependency.

# Status

The Gradualizer is close to a beta release. Most of the language constructs and
data types are handled, although there are things that don't work yet.

That being said, pull requests are most welcome!

A work-in-progress manual is located on the
[wiki](https://github.com/josefs/Gradualizer/wiki).

For a non-exhaustive list of known problems, see [test/known_problems/](test/known_problems/).
Additionally, these are things which need fixing:

* Support for intersection types
* Support for subtype polymorphism
