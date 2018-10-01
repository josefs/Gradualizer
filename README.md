# Gradualizer: A Gradual Type System for Erlang

This tool is a type checker for Erlang.

* The type system is based on the principles of Gradual Typing
* It works with the existsing type specs syntax in Erlang.
* Without any type specs, no static typing happens.

  When type specs are added the program the program is checked against
  these specs statically. The more type specs, the more static typing.

# Usage

To run Gradualizer from the command line:

* Compile the project as an escript:

  `make escript`

* Run Gradualizer:

  `gradualizer`

To run Gradualizer from the Erlang shell:

* Compile the project by:

  `make app`

* Launch the interactive prompt with all the relevant modules in the path:

  `make shell`

* From the prompt you can run the type checker as follows:

  `gradualizer:type_check_file(<path/to/some_file.erl>).`

  You can try typechecking some of the example modules in the `test` directory.

To run Gradualizer from rebar3, read `examples/rebar3/README.md`.

The Gradualizer requires at least OTP 19.

# Status

The Gradualizer is in alpha. There are plenty of things that don't work right
now. It is not meant for public consumption. That being said, pull requests
are most welcome!

A non-exhaustive list of things which need fixing:

* Integrate gradualizer with editors and build tools
* Support for intersection types
* Support for subtype polymorphism
* Support for all language constructs in Erlang
