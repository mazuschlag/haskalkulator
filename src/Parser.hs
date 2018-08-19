module Parser (Tree (..), parse) where

import Lexer (Token (..), Operator (..))
import Binder (bindE, pass, underachieve)

-- Parser --
data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
          | EndParse
  deriving (Show)

parse :: [Token] -> Either String Tree
parse toks = 
  case expression toks of
    Left msg -> underachieve msg
    Right (tree, toks') ->
      if null toks'
        then pass tree
        else underachieve $ "Unable to parse tokens " ++ show toks'

expression :: [Token] -> Either String (Tree, [Token])
expression toks = bindE (term toks) (\(termTree, toks') ->
  case peek toks' of
    -- Term [+-] Expression
    (TokOp op) | elem op [Plus, Minus] -> 
      bindE (expression (accept toks')) (\(exTree, toks'') -> 
        pass (SumNode op termTree exTree, toks''))
    -- Identifier '=' Expression
    TokAssign -> 
      case termTree of 
        VarNode str ->
          bindE (expression (accept toks')) (\(exTree, toks'') -> 
            pass (AssignNode str exTree, toks''))
        _ -> underachieve "Only variables can be assigned to"
    -- Term
    _ -> pass (termTree, toks'))

term :: [Token] -> Either String (Tree, [Token])
term toks = bindE (factor toks) (\(facTree, toks') ->
  -- Factor [*/] Term
  case peek toks' of
    (TokOp op) | elem op [Times, Div] ->
      bindE (term . accept $ toks') (\(termTree, toks'') ->
          pass (ProdNode op facTree termTree, toks''))
    -- Factor
    _ -> pass (facTree, toks'))

factor :: [Token] -> Either String (Tree, [Token])
factor toks = 
  case peek toks of 
    (TokNum x)     -> pass (NumNode x, accept toks)
    (TokIdent str) -> pass (VarNode str, accept toks)
    (TokOp op) | elem op [Plus, Minus] -> 
      bindE (factor . accept $ toks) (\(facTree, toks') ->
        pass (UnaryNode op facTree, toks'))
    TokLParen      -> bindE (expression . accept $ toks) (\(expTree, toks') ->
      if peek toks' /= TokRParen
      then underachieve "Missing right parenthesis"
      else pass (expTree, accept toks'))
    TokError -> underachieve $ "Unexpected end of input"
    _  -> underachieve $ "Parse error on token " ++ (show . peek $ toks)


peek :: [Token] -> Token
peek [] = TokError
peek (c:cs) = c

accept :: [Token] -> [Token]
accept [] = []
accept (t:ts) = ts