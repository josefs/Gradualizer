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
