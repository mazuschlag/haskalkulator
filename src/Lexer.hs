module Lexer (Operator (..), Token (..), tokenize) where

import Data.Char
import Binder (Binder (..))

-- Tokenizer --
data Token = TokLParen
           | TokRParen
           | TokAssign
           | TokOp Operator
           | TokIdent String
           | TokNum Double
           | TokError
           | TokEnd
  deriving (Show, Eq)

data Operator = Plus | Minus | Times | Div
  deriving (Show, Eq)

tokenize :: String -> Binder [Token]
tokenize [] = return []
tokenize (c : cs)
  | elem c "+-*/" = tokenize cs >>= (\toks -> return (TokOp (operator c) : toks))
  | c == '='      = tokenize cs >>= (\toks -> return (TokAssign : toks))
  | c == '('      = tokenize cs >>= (\toks -> return (TokLParen : toks))
  | c == ')'      = tokenize cs >>= (\toks -> return (TokRParen : toks))
  | isDigit c     = number c cs
  | isAlpha c     = identifier c cs
  | isSpace c     = tokenize cs
  | otherwise     = fail $ "Cannot tokenize " ++ [c]

number :: Char -> String -> Binder [Token]
number c cs = let (str, cs') = span isDigit cs in 
              tokenize cs' >>= (\toks -> return (TokNum (read (c:str)) : toks))

identifier :: Char -> String -> Binder [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in 
                  tokenize cs' >>= (\toks -> return (TokIdent (c:str) : toks))

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
