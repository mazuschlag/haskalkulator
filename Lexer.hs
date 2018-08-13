module Lexer (Operator (..), Token (..), tokenize) where

import Data.Char

-- Tokenizer --
data Token = TokLParen
           | TokRParen
           | TokAssign
           | TokOp Operator
           | TokIdent String
           | TokNum Double
           | TokEnd
  deriving (Show, Eq)

data Operator = Plus | Minus | Times | Div
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | elem c "+-*/" = TokOp (operator c) : tokenize cs
  | c == '='      = TokAssign          : tokenize cs
  | c == '('      = TokLParen          : tokenize cs
  | c == ')'      = TokRParen          : tokenize cs
  | isDigit c     = number c cs
  | isAlpha c     = identifier c cs
  | isSpace c     = tokenize cs
  | otherwise     = error $ "Cannot tokenize " ++ [c]

number :: Char -> String -> [Token]
number c cs = let (str, cs') = span isDigit cs in 
              TokNum (read (c:str)) : tokenize cs'

identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in 
                  TokIdent (c:str) : tokenize cs'

opToChar :: Operator -> Char
opToChar Plus = '+'
opToChar Minus = '-'
opToChar Times = '*'
opToChar Div = '/'

operator :: Char -> Operator
operator '+' = Plus
operator '-' = Minus
operator '*' = Times
operator '/' = Div
