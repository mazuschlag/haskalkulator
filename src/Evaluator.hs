module Evaluator (evaluate) where

import qualified Data.Map as M
import Lexer (Operator (..))
import Parser (Tree (..))
import Binder (Binder (..))
  
-- Evaluator --
type SymTab = M.Map String Double

evaluate :: Tree -> SymTab -> Binder (Double, SymTab)
evaluate (SumNode op left right) symTab =
  evaluate left symTab >>= (\(x, symTab') -> 
    evaluate right symTab' >>= (\(y, symTab'') ->
      case op of
        Plus -> return (x + y, symTab'')
        Minus -> return (x - y, symTab'')))

evaluate (ProdNode op left right) symTab =
  evaluate left symTab >>= (\(x, symTab') ->
    evaluate right symTab' >>= (\(y, symTab'') -> 
      case op of 
        Times -> return (x * y, symTab'')
        Div -> return (x / y, symTab'')))

evaluate (UnaryNode op tree) symTab = 
  evaluate tree symTab >>= (\(x, symTab')
    -> case op of
      Plus -> return (x, symTab')
      Minus -> return (-x, symTab'))

evaluate (NumNode x) symTab = return (x, symTab)

evaluate (AssignNode str tree) symTab = 
  evaluate tree symTab >>= (\(x, symTab') ->
    return (addSymbol str x symTab'))

evaluate (VarNode str) symTab = lookUp str symTab

lookUp :: String -> SymTab -> Binder (Double, SymTab)
lookUp str symTab = 
  case M.lookup str symTab of 
    Just v -> return (v, symTab)
    Nothing -> fail $ "Undefined variable " ++ str

addSymbol :: String -> Double -> SymTab -> (Double, SymTab)
addSymbol str val symTab =
  let symTab' = M.insert str val symTab
  in (val, symTab')