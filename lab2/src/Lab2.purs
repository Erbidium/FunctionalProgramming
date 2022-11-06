module Lab2
  ( findIndex
  , test
  )
  where

import Prelude

import Data.List (List(Cons, Nil))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

infixr 6 Cons as :

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex predicate (x : xs) | predicate x = Just (0)
                             | otherwise = case findIndex predicate xs of
                                            Just (index) -> Just (index + 1)
                                            _ -> Nothing
findIndex _ _ = Nothing

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex predicate (x : xs) = case findLastIndex predicate xs of
                                    Just (index) -> Just (index + 1)
                                    _ -> if predicate x 
                                            then Just (0)
                                            else Nothing
findLastIndex _ _ = Nothing


test::Effect Unit
test = do
  log $ show $ findIndex (\x -> x == 5) (5: 5 : 5 : Nil)
  log $ show $ findLastIndex (\x -> x == 5) (5: 5 : 5 : Nil)