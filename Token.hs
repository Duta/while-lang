module Token where

data Token
  = T_Word String
  | T_Int  Int
  | T_LCurly
  | T_RCurly
  | T_LParen
  | T_RParen
  | T_LessThan
  | T_GreaterThan
  | T_Colon
  | T_Semicolon
  | T_Equal
    deriving (Eq, Show)
