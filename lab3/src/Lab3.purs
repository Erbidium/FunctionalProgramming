module Lab3 where

import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, compare, discard, show, ($), (<), (<=), (==), (>), (>=))


data Maybe a = Nothing | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false

instance compareMaybe ::Ord Maybe where
  compare (Just a) (Just b) = compare a b
  compare (Just a) Nothing = GT
  compare Nothing (Just a) = LT
  compare Nothing Nothing = EQ



greaterThanOrEq :: forall a. Ord a => a -> a -> Boolean
greaterThanOrEq a1 a2 = case a1 `compare` a2 of
  GT -> true
  EQ -> true
  _ -> false

greaterThan :: forall a. Ord a => a -> a -> Boolean
greaterThan a1 a2 = case a1 `compare` a2 of
  GT -> true
  _ -> false

lessThanOrEq :: forall a. Ord a => a -> a -> Boolean
lessThanOrEq a1 a2 = case a1 `compare` a2 of
  LT -> true
  EQ -> true
  _ -> false

lessThan :: forall a. Ord a => a -> a -> Boolean
lessThan a1 a2 = case a1 `compare` a2 of
  LT -> true
  _ -> false

infixl 4 greaterThanOrEq as >=
infixl 4 greaterThan as >
infixl 4 lessThanOrEq as <=
infixl 4 lessThan as <


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




