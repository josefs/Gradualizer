# Gradualizer: A Gradual Type System for Erlang

This tool is a type checker for Erlang.

* The type system is based on the principles of Gradual Typing
* It works with the existsing type specs syntax in Erlang.
* Without any type specs, no static typing happens.

  When type specs are added the program the program is checked against
  these specs statically. The more type specs, the more static typing.

# Usage

Right now the user experience is not very polished. Here's how to get started:

* Type `make` in the root directory of the repository.

  This will launch the interactive prompt with all the relevant modules loaded.

* From the prompt you can run the type checker as follows:

  `typechecker:type_check_file(<some file>).`

  You can try typechecking some of the example modules in the `tests`directory.

The Gradualizer requires at least OTP 19.

# Mix

Project is compatible with Elixir `mix` utility:

```
$ mix deps.get       # fetch dependencies (including mix-erlang-tasks)

$ mix compile        # compile everything

$ mix eunit          # run EUnit tests

$ mix ct             # run Common Test suites

$ mix edoc           # generate HTML documentation from the source
```

# Status

The Gradualizer is in alpha. There are plenty of things that don't work right
now. It is not ment for public consumption. That being said, pull requrests
are most welcome!

A non-exhaustive list of things which need fixing:

* Making it a real Erlang application
* Support for intersection types
* Support for subtype polymorphism
* Support for all language constructs in Erlang
