module Evaluator (evaluate) where

import qualified Data.Map as M
import Lexer (Operator (..))
import Parser (Tree (..))
  
-- Evaluator --
type SymTab = M.Map String Double

evaluate :: Tree -> SymTab -> Either String (Double, SymTab)
evaluate (SumNode op left right) symTab =
  bindE (evaluate left symTab) (\(x, symTab') -> 
    bindE (evaluate right symTab') (\(y, symTab'') ->
      case op of
        Plus -> pass (x + y, symTab'')
        Minus -> pass (x - y, symTab'')))

evaluate (ProdNode op left right) symTab =
  bindE (evaluate left symTab) (\(x, symTab') ->
    bindE (evaluate right symTab') (\(y, symTab'') -> 
      case op of 
        Times -> pass (x * y, symTab'')
        Div -> pass (x / y, symTab'')))

evaluate (UnaryNode op tree) symTab = 
  bindE (evaluate tree symTab) (\(x, symTab')
    -> case op of
      Plus -> pass (x, symTab')
      Minus -> pass (-x, symTab'))

evaluate (NumNode x) symTab = pass (x, symTab)

evaluate (AssignNode str tree) symTab = 
  bindE (evaluate tree symTab) (\(x, symTab') ->
    pass (addSymbol str x symTab'))

evaluate (VarNode str) symTab = lookUp str symTab

lookUp :: String -> SymTab -> Either String (Double, SymTab)
lookUp str symTab = 
  case M.lookup str symTab of 
    Just v -> pass (v, symTab)
    Nothing -> underachieve $ "Undefined variable " ++ str

addSymbol :: String -> Double -> SymTab -> (Double, SymTab)
addSymbol str val symTab =
  let symTab' = M.insert str val symTab
  in (val, symTab')

bindE :: Either String a -> (a -> Either String a) -> Either String a
bindE ev k = 
  case ev of 
    Left msg -> Left msg
    Right v -> k v

pass :: a -> Either String a
pass x = Right x

underachieve :: String -> Either String a
underachieve msg = Left msg