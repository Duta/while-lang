while-lang
==========

Simple language demonstrating the compiler pipeline.

Frontend:
---------

*Text* -> **Lex** -> *Tokens* -> **Parse** -> *Full AST* -> **Simplify AST** -> *Simple AST* -> **Typechecker** -> *Simple AST* -> **Optimize** -> *Simple AST*

Backend:
--------

*Simple AST* -> **Interpreter** -> *Result*

*Simple AST* -> **Compiler** -> *Bytecode* -> **VM** -> *Result*

*Simple AST* -> **Compiler** -> *Machine code* -> **Native execution** -> *Result*
