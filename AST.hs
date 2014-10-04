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
  | M_IfElse MiniExpr MiniAST MiniAST
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
reduce ast = case ast of
  F_Seq asts ->
    M_Seq $ map reduce asts
  F_If cond ast ->
    reduce $ F_IfElse cond ast (F_Seq [])
  F_IfElse cond ast1 ast2 ->
    M_IfElse (reduceExpr cond) (reduce ast1) (reduce ast2)
  F_While cond ast ->
    M_While (reduceExpr cond) (reduce ast)
  F_Assign var expr ->
    M_Assign var (reduceExpr expr)

reduceExpr :: FullExpr -> MiniExpr
reduceExpr expr = case expr of
  F_UOp op expr -> (case op of
    F_Not -> M_UOp M_Not expr'
    F_Neg -> M_UOp M_Neg expr')
    where
      expr' = reduceExpr expr
  F_BOp op expr1 expr2 -> (case op of
    F_Add -> M_BOp M_Add expr1' expr2'
    F_Sub -> M_BOp M_Sub expr1' expr2'
    F_Mul -> M_BOp M_Mul expr1' expr2'
    F_Div -> M_BOp M_Div expr1' expr2'
    F_Eq  -> M_BOp M_Eq  expr1' expr2'
    F_Neq -> M_UOp M_Not (M_BOp M_Eq expr1' expr2'))
    where
      expr1' = reduceExpr expr1
      expr2' = reduceExpr expr2
  F_Var var ->
    M_Var var
  F_Int n ->
    M_Int n
  F_Bool b ->
    M_Bool b
