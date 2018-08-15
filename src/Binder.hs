module Binder (bindE, pass, underachieve) where

bindE :: Either String a -> (a -> Either String a) -> Either String a
bindE ev k = 
  case ev of 
    Left msg -> Left msg
    Right v -> k v

pass :: a -> Either String a
pass x = Right x

underachieve :: String -> Either String a
underachieve msg = Left msg