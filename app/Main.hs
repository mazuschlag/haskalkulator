module Main where

import qualified Data.Map as M
import Lexer (tokenize)
import Parser (parse)
import Evaluator (evaluate)
import Binder (Binder (..))

main :: IO ()
main = do
  loop (M.fromList [("pi", pi), ("e", exp 1.0)])
  where
    loop symTab = do
      str <- getLine
      if null str
        then return ()
      else
        let result = tokenize str >>= (\toks -> parse toks >>= (\tree -> evaluate tree symTab)) in
        case result of 
          Bind (Left msg) -> do
            putStrLn $ "Error: " ++ msg
            loop symTab
          Bind (Right (val, symTab')) -> do
            print val
            loop symTab'