module Main where

import qualified Data.Map as M
import Lexer (tokenize)
import Parser (parse)
import Evaluator (evaluate)

main :: IO ()
main = do
  loop (M.fromList [("pi", pi), ("e", exp 1.0)])
  where
    loop symTab = do
      str <- getLine
      if null str
        then return ()
      else
        case tokenize str of 
          Left msg -> do
            putStrLn $ "Error: " ++ msg
            loop symTab
          Right toks -> do
            case parse toks of 
              Left msg -> do
                putStrLn $ "Error: " ++ msg
                loop symTab
              Right tree -> do
                case evaluate tree symTab of
                  Left msg -> do
                    putStrLn $ "Error: " ++ msg
                    loop symTab
                  Right (val, symTab') -> do
                    print val
                    loop symTab'