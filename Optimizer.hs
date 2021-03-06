module Optimizer (optimize) where

import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

import AST (MiniAST(..), MiniExpr(..), M_UOp(..), M_BOp(..))


optimize :: [MiniAST] -> [MiniAST]
optimize = constantFold

-- Also does some /very/ basic dead code elim.
constantFold :: [MiniAST] -> [MiniAST]
constantFold = concatMap (toList . constantFold')
  where
    constantFold' :: MiniAST -> Maybe MiniAST
    constantFold' ast = (case ast of
      M_Seq asts -> case constantFold asts of
        [] -> Nothing
        xs -> Just $ M_Seq xs
      M_IfElse cond ast1 ast2 -> (case constantFoldExpr cond of
        M_Bool True  -> ast1'
        M_Bool False -> ast2'
        cond'        -> case (ast1', ast2') of
          (Nothing, Nothing) -> Nothing
          _                  -> Just $ M_IfElse cond' (fromMaybe' ast1') (fromMaybe' ast2'))
        where
          ast1' = constantFold' ast1
          ast2' = constantFold' ast2
      M_While cond ast -> case constantFoldExpr cond of
        M_Bool False -> Nothing
        cond'        -> Just $ M_While cond' (fromMaybe' $ constantFold' ast)
      M_Assign var expr ->
        Just $ M_Assign var (constantFoldExpr expr))
      where
        fromMaybe' :: Maybe MiniAST -> MiniAST
        fromMaybe' = fromMaybe (M_Seq [])

constantFoldExpr :: MiniExpr -> MiniExpr
constantFoldExpr expr = case expr of
  M_UOp op expr -> (case op of
    M_Not -> case expr' of
      M_Bool b -> M_Bool $ not b
      _        -> M_UOp op expr'
    M_Neg -> case expr' of
      M_Int n -> M_Int $ negate n
      _       -> M_UOp op expr')
    where
      expr' = constantFoldExpr expr
  M_BOp op expr1 expr2 -> (case op of
    M_Add -> case (expr1', expr2') of
      (M_Int m, M_Int n)   -> M_Int $ m + n
      _                    -> M_BOp op expr1' expr2'
    M_Sub -> case (expr1', expr2') of
      (M_Int m, M_Int n)   -> M_Int $ m - n
      _                    -> M_BOp op expr1' expr2'
    M_Mul -> case (expr1', expr2') of
      (M_Int m, M_Int n)   -> M_Int $ m * n
      _                    -> M_BOp op expr1' expr2'
    M_Div -> case (expr1', expr2') of
      (M_Int m, M_Int n)   -> M_Int $ m `div` n
      _                    -> M_BOp op expr1' expr2'
    M_And -> case (expr1', expr2') of
      (M_Bool m, M_Bool n) -> M_Bool $ m && n
      _                    -> M_BOp op expr1' expr2'
    M_Or  -> case (expr1', expr2') of
      (M_Bool m, M_Bool n) -> M_Bool $ m || n
      _                    -> M_BOp op expr1' expr2'
    M_Eq  -> case (expr1', expr2') of
      (M_Int m, M_Int n)   -> M_Bool $ m == n
      (M_Bool m, M_Bool n) -> M_Bool $ m == n
      _                    -> M_BOp op expr1' expr2'
    )
    where
      expr1' = constantFoldExpr expr1
      expr2' = constantFoldExpr expr2
  expr -> expr
