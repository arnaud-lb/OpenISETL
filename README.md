OpenISETL
=========

Scala/JVM implementation of the ISETL language.

Why?
----

This is a toy project found on an old hard drive, had lot of fun doing this.

The language
------------

ISETL is a dynamically typed, mostly functional language with first class functions, mutable variables, immutable values, closures with lexical scope, destructuring assignments, list comprehensions, native set manipulation syntax, and many other fun features.

### Implemented features

- Binary ops: +, union, -, /, div, *, inter, mod, ?, with, less, >, <, >=, <=, =, /=, iff, and, or, impl, subset, in, notin
- Types: bool, int, bignum, real, set, tuple, undefined, func
- Control structures: if/elseif/else
- Anonymous functions, closures with lexical variables

### Non implemented features

- Unary ops: +, -, not, #
- Control structures: for, forall, choose, exists, while
- Destructuring assignments

The Implementation
------------------

The code is [parsed](https://github.com/arnaud-lb/OpenISETL/blob/master/src/main/scala/openisetl/compiler/parser/parser.scala) into an [AST](https://github.com/arnaud-lb/OpenISETL/blob/master/src/main/scala/openisetl/compiler/node/Nodes.scala) using Scala's parsing.combinator package. A [few](https://github.com/arnaud-lb/OpenISETL/tree/master/src/main/scala/openisetl/compiler/analysis) analysis and transformations are applied to the AST, JVM classes are [generated](https://github.com/arnaud-lb/OpenISETL/blob/master/src/main/scala/openisetl/compiler/CodeGen.scala), and eventually executed.

The language is dynamic, so, to make things simple, every runtime value is an object implementing the same interface: ISETL types are [sub-classes](https://github.com/arnaud-lb/OpenISETL/tree/master/src/main/scala/openisetl/runtime/_val) of the [same ancestor](https://github.com/arnaud-lb/OpenISETL/blob/master/src/main/scala/openisetl/runtime/_val/GenericBaseVal.scala), and values are instances of these. Operators are implemented as methods on the types, so that compiling an operation between two values is as simple as emitting bytecode for calling a method on the first value taking the second value as parameter.

Functions are values, too. They implement a variation of a call() method (taking more or less parameters). The call method is ISETL code compiled to JVM byte code. The variables accessed by a function are known at compile time, so a closure doesn't need a reference to the whole environment. Closed variables are referenced in instance members. Locale variables and arguments are true JVM locales and arguments.


