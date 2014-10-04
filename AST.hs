module AST where

type Identifier = String

data FullAST
  = F_Seq [FullAST]
  | F_If FullExpr FullAST
  | F_IfElse FullExpr FullAST FullAST
  | F_While FullExpr FullAST
  | F_Assign Identifier FullExpr
    deriving (Eq, Show)

data FullExpr
  = F_UOp F_UOp FullExpr
  | F_BOp F_BOp FullExpr FullExpr
  | F_Var Identifier
  | F_Int Int
  | F_Bool Bool
    deriving (Eq, Show)

data F_UOp
  = F_Not
  | F_Neg
    deriving (Eq, Show)

data F_BOp
  = F_Add
  | F_Sub
  | F_Mul
  | F_Div
  | F_Eq
  | F_Neq
    deriving (Eq, Show)

data MiniAST
  = M_Seq [MiniAST]
  | M_IfElse MiniExpr MiniExpr MiniExpr
  | M_While MiniExpr MiniAST
  | M_Assign Identifier MiniExpr
    deriving (Eq, Show)

data MiniExpr
  = M_UOp M_UOp MiniExpr
  | M_BOp M_BOp MiniExpr MiniExpr
  | M_Var Identifier
  | M_Int Int
  | M_Bool Bool
    deriving (Eq, Show)

data M_UOp
  = M_Not
  | M_Neg
    deriving (Eq, Show)

data M_BOp
  = M_Add
  | M_Sub
  | M_Mul
  | M_Div
  | M_Eq
    deriving (Eq, Show)

reduce :: FullAST -> MiniAST
reduce = error "AST reduction not implemented"
