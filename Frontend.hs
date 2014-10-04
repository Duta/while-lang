module Frontend (toAST) where

import Prelude hiding (lex)

import AST (MiniAST, reduce)
import Lexer (lex)
import Parser (parse)
import Optimizer (optimize)

toAST :: String -> Maybe [MiniAST]
toAST input = lex input >>= parse >>= return . optimize . map reduce
