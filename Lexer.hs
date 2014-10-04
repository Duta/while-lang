module Lexer (lex) where

import Control.Arrow (second)
import Control.Applicative ((<$>))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Foldable (Foldable, foldr)
import Prelude hiding (lex, foldr)

import Token (Token(..))

type Folder a b = a -> b -> b
type LexResult  = Maybe (Token, String)
type Lexer      = String -> LexResult

lex :: String -> Maybe [Token]
lex = useLexers
  [ matchSymbol
  , matchInt
  , matchIdentifier
  ]

useLexers :: Foldable t => t Lexer -> String -> Maybe [Token]
useLexers lexers = useLexers' . skipWhitespace
  where
    useLexers' :: String -> Maybe [Token]
    useLexers' ""    = Just []
    useLexers' input = do
        (token, input') <- foldr useLexer Nothing lexers
        (token:) <$> useLexers lexers input'
      where
        useLexer :: Folder Lexer LexResult
        useLexer lexer Nothing = lexer input
        useLexer lexer match = match

skipWhitespace :: String -> String
skipWhitespace = dropWhile (`elem` " \t\n\r")

matchKeys :: Foldable t => t (String, Token) -> Lexer
matchKeys keys input = foldr matchKey Nothing keys
  where
    matchKey :: Folder (String, Token) LexResult
    matchKey (key, value) Nothing
      | matches   = Just (value, input')
      | otherwise = Nothing
      where
        matches :: Bool
        matches = and $ zipWith (==) key input
        input' :: String
        input' = drop (length key) input
    matchKey key match = match

matchSymbol :: Lexer
matchSymbol = matchKeys
  [ ("{", T_LCurly)
  , ("}", T_RCurly)
  , ("(", T_LParen)
  , (")", T_RParen)
  , ("<", T_LessThan)
  , (">", T_GreaterThan)
  , (":", T_Colon)
  , (";", T_Semicolon)
  , ("=", T_Equal)
  ]

matchRule :: (Char -> Bool) -> (Char -> Bool) -> (String -> Token) -> Lexer
matchRule validStart validPart toToken "" = Nothing
matchRule validStart validPart toToken (c:input)
  | validStart c = let (cs, input') = span validPart input
                   in Just (toToken $ c:cs, input')
  | otherwise    = Nothing

oneOf :: [a -> Bool] -> a -> Bool
oneOf fs x = any ($x) fs

matchIdentifier :: Lexer
matchIdentifier = matchRule validStart validPart T_Word
  where
    validStart, validPart :: Char -> Bool
    validStart = oneOf [(=='_'), (=='-'), isAlpha]
    validPart  = oneOf [(=='_'), (=='-'), isAlphaNum]

matchInt :: Lexer
matchInt = matchRule isDigit isDigit (T_Int . read)
