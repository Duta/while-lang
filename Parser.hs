module Parser (parse) where

import AST (FullAST(..))
import Token (Token(..))

parse :: [Token] -> Maybe FullAST
parse = error "Parsing not implemented"
