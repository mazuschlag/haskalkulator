module Parser (Tree (..), parse) where

import Lexer (Token (..), Operator (..))
import Binder (Binder (..))

-- Parser --
data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
          | EndParse
  deriving (Show)

parse :: [Token] -> Binder Tree
parse toks = 
  case expression toks of
    Bind (Left msg) -> fail msg
    Bind (Right (tree, toks')) ->
      if null toks'
        then return tree
        else fail $ "Unable to parse tokens " ++ show toks'

expression :: [Token] -> Binder (Tree, [Token])
expression toks = term toks >>= (\(termTree, toks') ->
  case peek toks' of
    -- Term [+-] Expression
    (TokOp op) | elem op [Plus, Minus] -> 
      expression (accept toks') >>= (\(exTree, toks'') -> 
        return (SumNode op termTree exTree, toks''))
    -- Identifier '=' Expression
    TokAssign -> 
      case termTree of 
        VarNode str ->
          expression (accept toks') >>= (\(exTree, toks'') -> 
            return (AssignNode str exTree, toks''))
        _ -> fail "Only variables can be assigned to"
    -- Term
    _ -> return (termTree, toks'))

term :: [Token] -> Binder (Tree, [Token])
term toks = factor toks >>= (\(facTree, toks') ->
  -- Factor [*/] Term
  case peek toks' of
    (TokOp op) | elem op [Times, Div] ->
      (term . accept $ toks') >>= (\(termTree, toks'') ->
          return (ProdNode op facTree termTree, toks''))
    -- Factor
    _ -> return (facTree, toks'))

factor :: [Token] -> Binder (Tree, [Token])
factor toks = 
  case peek toks of 
    (TokNum x)     -> return (NumNode x, accept toks)
    (TokIdent str) -> return (VarNode str, accept toks)
    (TokOp op) | elem op [Plus, Minus] -> 
      (factor . accept $ toks) >>= (\(facTree, toks') ->
        return (UnaryNode op facTree, toks'))
    TokLParen      -> (expression . accept $ toks) >>= (\(expTree, toks') ->
      if peek toks' /= TokRParen
      then fail "Missing right parenthesis"
      else return (expTree, accept toks'))
    TokError -> fail $ "Unexpected end of input"
    _  -> fail $ "Parse error on token " ++ (show . peek $ toks)


peek :: [Token] -> Token
peek [] = TokError
peek (c:cs) = c

accept :: [Token] -> [Token]
accept [] = []
accept (t:ts) = ts