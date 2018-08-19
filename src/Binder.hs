module Binder (Binder (..), bindE, pass, underachieve) where

import Control.Applicative
import Control.Monad (liftM, ap)

newtype Binder a = Bind (Either String a)

instance Functor Binder where
  fmap f = liftM f

instance Applicative Binder where
  pure = return
  (<*>) = ap

instance Monad Binder where
  (Bind ev) >>= k =
    case ev of 
      Left msg -> Bind (Left msg)
      Right v -> k v
  return v = Bind (Right v)
  fail msg = Bind (Left msg)

bindE :: Either String a -> (a -> Either String a) -> Either String a
bindE ev k = 
  case ev of 
    Left msg -> Left msg
    Right v -> k v

pass :: a -> Either String a
pass x = Right x

underachieve :: String -> Either String a
underachieve msg = Left msg