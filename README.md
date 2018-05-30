# A Gradual Type System for Erlang

This tool is a type checker for Erlang.

* The type system is based on the principles of Gradual Typing
* It works with the existsing type specs syntax in Erlang.
* Without any type specs, no static typing happens.

  When type specs are added the program the program is checked against
  these specs statically. The more type specs, the more static typing.
