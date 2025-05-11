# C to Brainfuck

This is a compiler from a subset of C to highly unoptimized Brainfuck. There are no support for dynamic allocations, pointers and global variables. Everything else can be derived from the present:
- Types: 
  - `int` -- the size of bf machine cell, e.g. 8 bits
  - `T[N]` -- arrays of type `T` with statically known size `N`
  - `struct { T f; ... }` -- custom structs

  Types can be defined as usual with `typedef struct { .. } S;`
- Functions:
  - `T func(T arg, ...);` -- declaration
  - `T func(T arg, ...) { .. }` -- definition
- Statements:
  - `T x = expr` -- local variables declarations
  - `x = expr` -- assignments
  - `if (expr) { .. } else { .. }` -- conditionals
  - `while (expr) { .. }` -- loop construcions
  - `func(expr, ...)` -- function calls
- Expressions
  - `x` -- refering to local variables and arguments
  - `expr.field` -- accessing structures' fields
  - `x[expr]` -- indexing arrays with custom expressions
  - `func(expr, ...)` -- calling functions
  - `expr + expr`, `expr - expr` -- wrapping on overvflow addition and subtraction

All features are presented in example [implementation of Rule 110](./examples/rule110.c) ([Run compiled version](https://copy.sh/brainfuck/?file=https://raw.githubusercontent.com/iliayar/cbf/refs/heads/master/examples/rule110.bf)).
