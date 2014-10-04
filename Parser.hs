module Parser (parse) where

import Control.Applicative ((<$>), (<|>), many)
import Control.Monad.State (StateT, get, put, runStateT, evalStateT)

import AST (Identifier, FullAST(..), FullExpr(..), F_UOp(..), F_BOp(..))
import Token (Token(..))

type Parser r    = StateT [Token] Maybe r

parse :: [Token] -> Maybe [FullAST]
parse input = evalStateT (many parseAST) input

reqToken' :: (Token -> Bool) -> Parser Token
reqToken' p = do
  input <- get
  case input of
    [] -> fail ""
    (t:input') ->
      if p t
      then put input' >> return t
      else fail ""

reqToken :: Token -> Parser Token
reqToken = reqToken' . (==)

reqTokens :: [Token] -> Parser [Token]
reqTokens = mapM reqToken

reserved :: String -> Parser Token
reserved = reqToken . T_Word

parseAST :: Parser FullAST
parseAST = parseSeq
       <|> parseIfElse
       <|> parseIf
       <|> parseWhile
       <|> parseAssign

parseSeq :: Parser FullAST
parseSeq = do
  reqToken T_LCurly
  asts <- many parseAST
  reqToken T_RCurly
  return $ F_Seq asts

parseIfElse :: Parser FullAST
parseIfElse = do
  reserved "if"
  cond <- parseExpr
  ast1 <- parseAST
  reserved "else"
  ast2 <- parseAST
  return $ F_IfElse cond ast1 ast2

parseIf :: Parser FullAST
parseIf = do
  reserved "if"
  cond <- parseExpr
  ast  <- parseAST
  return $ F_If cond ast

parseWhile :: Parser FullAST
parseWhile = do
  reserved "while"
  cond <- parseExpr
  ast  <- parseAST
  return $ F_While cond ast

parseAssign :: Parser FullAST
parseAssign = do
  var  <- identifier
  reqToken T_Colon
  reqToken T_Equal
  expr <- parseExpr
  reqToken T_Semicolon
  return $ F_Assign var expr

parseExpr :: Parser FullExpr
parseExpr = parseUOp
        <|> parseVar
        <|> parseInt
        <|> parseBool

parseBOp :: Parser FullExpr
parseBOp = do
  expr1 <- parseExpr
  op    <- infixOp
  expr2 <- parseExpr
  return $ F_BOp op expr1 expr2

parseUOp :: Parser FullExpr
parseUOp = do
  op   <- prefixOp
  expr <- parseExpr
  return $ F_UOp op expr

prefixOp :: Parser F_UOp
prefixOp = (reqToken T_Bang  >> return F_Not)
       <|> (reqToken T_Minus >> return F_Neg)

infixOp :: Parser F_BOp
infixOp = (reqToken T_Plus  >> return F_Add)
      <|> (reqToken T_Minus >> return F_Sub)
      <|> (reqToken T_Mul   >> return F_Mul)
      <|> (reqToken T_Div   >> return F_Div)
      <|> (reqToken T_Equal >> return F_Eq)
      <|> (reqTokens [T_Bang, T_Equal] >> return F_Neq)

parseVar :: Parser FullExpr
parseVar = F_Var <$> identifier

parseBool :: Parser FullExpr
parseBool = (reserved "true"  >> return (F_Bool True))
        <|> (reserved "false" >> return (F_Bool False))

parseInt :: Parser FullExpr
parseInt = F_Int . unwrapID <$> reqToken' isID
  where -- Hacky as fuck
    unwrapID (T_Int var) = var
    unwrapID _           = error "Parser.parseInt: Should never happen"
    isID (T_Int{}) = True
    isID _         = False

identifier :: Parser Identifier
identifier = unwrapID <$> reqToken' isID
  where -- Hacky as fuck
    unwrapID (T_Word var) = var
    unwrapID _            = error "Parser.identifier: Should never happen"
    isID (T_Word{}) = True
    isID _          = False
