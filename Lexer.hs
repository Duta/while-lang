module Lexer (lex) where

import Prelude hiding (lex)

import Token (Token(..))

lex :: String -> Maybe [Token]
lex = error "Lexing not implemented"