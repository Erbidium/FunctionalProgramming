module Lab3 where

import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (>=))
import Data.Eq (class Eq)
import Effect (Effect)
import Effect.Console (log)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)

data Maybe a = Nothing | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false

data Ordering = LT | GT | EQ

compare :: a -> a -> Ordering

greaterThanOrEq :: forall a. Ord a => a -> a -> Boolean


infixl 4 greaterThanOrEq as >=


test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "------------------"
  log $ show $ Just 1 < Just 5 -- COMPILER ERROR!!
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log $ show $ Just "abc"
  log $ show $ (Nothing :: Maybe Unit)




