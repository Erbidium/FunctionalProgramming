module Lab1 (test) where

import Prelude

import Data.List (List(Cons, Nil))
import Effect (Effect)
import Effect.Console (log)

infixr 6 Cons as :

singleton :: forall a. a -> List a
singleton a = a : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc list item = case list of
    Nil -> item : Nil
    head : tail -> head : snoc tail item

length :: forall a. List a -> Int
length list = case list of
    Nil -> 0
    _ : tail -> 1 + length tail


test::Effect Unit
test = do
  log $ show $ singleton 5
  log $ show $ null (5 : Nil)
  log $ show $ snoc (5 : Nil) 5
  log $ show $ length (5 : 5 : 5 : 5 : Nil)