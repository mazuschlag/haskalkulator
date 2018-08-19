module Lexer (Operator (..), Token (..), tokenize) where

import Data.Char
import Binder (bindE, pass, underachieve)

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

tokenize :: String -> Either String [Token]
tokenize [] = pass []
tokenize (c : cs)
  | elem c "+-*/" = bindE (tokenize cs) (\toks -> pass (TokOp (operator c) : toks))
  | c == '='      = bindE (tokenize cs) (\toks -> pass (TokAssign : toks))
  | c == '('      = bindE (tokenize cs) (\toks -> pass (TokLParen : toks))
  | c == ')'      = bindE (tokenize cs) (\toks -> pass (TokRParen : toks))
  | isDigit c     = number c cs
  | isAlpha c     = identifier c cs
  | isSpace c     = tokenize cs
  | otherwise     = underachieve $ "Cannot tokenize " ++ [c]

number :: Char -> String -> Either String [Token]
number c cs = let (str, cs') = span isDigit cs in 
              bindE (tokenize cs') (\toks -> pass (TokNum (read (c:str)) : toks))

identifier :: Char -> String -> Either String [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in 
                  bindE (tokenize cs') (\toks -> pass (TokIdent (c:str) : toks))

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
