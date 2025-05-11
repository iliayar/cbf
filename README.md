# C to Brainfuck

This is a compiler from a subset of C to highly unoptimized Brainfuck. There are no support for dynamic allocations, pointers and global variables. Everything else can be derived from the present:
- Types: 
  - `int` -- the size of bf machine cell, e.g. 8 bits
  - `T[N]` -- arrays of type `T` with statically known size `N`
  - `struct { T f; ... }` -- custom structs

  Types can be defined as usual with `typedef struct { .. } S;`
- Functions: local variables declarations, conditional and loop construcions, function calls
- Expressions: refering to local variables and arguments, accessing structures' fields, indexing arrays with custom expressions, calling functions, addition and subtraction

All features are presented in example [implementation of Rule 110](./examples/rule110.c).
