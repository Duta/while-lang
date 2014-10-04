while-lang
==========

Simple language demonstrating the compiler pipeline.

Frontend:
---------

*Text* -> **Lexer** -> *Tokens* -> **Parser** -> *Full AST* -> **AST Simplifier** -> *Simple AST* -> **Typechecker** -> *Simple AST* -> **Optimizer** -> *Simple AST*

Backend:
--------

*Simple AST* -> **Interpreter** -> *Result*

*Simple AST* -> **Compiler** -> *Bytecode*

*Simple AST* -> **Compiler** -> *Machine code*

Later:
------

*Bytecode* -> **VM** -> *Result*

*Machine code* -> **Machine** -> *Result*
