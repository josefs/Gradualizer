<h1 align="center">Gradualizer: A Gradual Type System for Erlang</h1>
<h2 align="center">A type checker for Erlang</h2>
<p align="center">
  <a href="https://travis-ci.com/josefs/Gradualizer">
    <img src="https://img.shields.io/travis/com/josefs/Gradualizer/master.svg?style=flat-square" alt="Travis CI" />
  </a>
  <a href="https://codecov.io/gh/josefs/Gradualizer">
    <img src="https://img.shields.io/codecov/c/github/josefs/Gradualizer/master.svg?style=flat-square" alt="Codecov" />
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

To run Gradualizer from the command line:

* Compile the project as an escript:

  `make escript`

* Run Gradualizer:

  `./gradualizer`

To run Gradualizer from the Erlang shell:

* Launch the interactive prompt with all the relevant modules in the path:

  `make shell`

* From the prompt you can run the type checker as follows:

  `gradualizer:type_check_file(<path/to/some_file.erl>).`

  You can try typechecking some of the example modules in the `test` directory.

To run Gradualizer from rebar3, read `examples/rebar3/README.md`.

The Gradualizer requires at least OTP 19 and is built using [rebar3](https://www.rebar3.org/).

# Status

The Gradualizer is in alpha. There are plenty of things that don't work right
now. It is not meant for public consumption. That being said, pull requests
are most welcome!

A non-exhaustive list of things which need fixing:

* Integrate gradualizer with editors and build tools
* Support for intersection types
* Support for subtype polymorphism
* Support for all language constructs in Erlang
