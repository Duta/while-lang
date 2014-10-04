module Optimizer (optimize) where

import AST (MiniAST(..))

optimize :: [MiniAST] -> [MiniAST]
optimize = id
