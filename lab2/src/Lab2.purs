module Lab2
  ( findIndex
  , test
  )
  where

import Prelude

import Data.List (List(Cons, Nil), reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip (x1 : Nil) (x2 : _) = ((Tuple x1 x2) : Nil)
zip (x1 : _) (x2 : Nil) = ((Tuple x1 x2) : Nil)
zip (x1 : xs1) (x2 : xs2) = ((Tuple x1 x2) : (zip xs1 xs2))
zip _ _ = Nil

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip (Tuple x1 x2 : xs) = Tuple (x1 : xs1) (x2 : xs2)
    where (Tuple xs1 xs2) = unzip xs
unzip _ = Tuple Nil Nil

filter :: forall a. (a -> Boolean) -> List a -> List a
filter predicate (x : xs) | predicate x = (x : (filter predicate xs))
                          | otherwise = (filter predicate xs)
filter _ _ = Nil

filterTail :: forall a. (a -> Boolean) -> List a -> List a
filterTail = listAccum Nil
    where
        listAccum :: forall b. List b -> (b -> Boolean) -> List b -> List b
        listAccum accum predicate (x : xs) | predicate x = listAccum (x : accum) predicate xs
                                           | otherwise = listAccum accum predicate xs
        listAccum accum _ _ = reverse accum

take :: forall a. Int -> List a -> List a
take _ Nil = Nil
take count (x : xs)
    | count <= 0 = Nil
    | otherwise = x : (take (count - 1) xs)

test::Effect Unit
test = do
  log $ show $ findIndex (\x -> x == 5) (5: 5 : 5 : Nil)
  log $ show $ findLastIndex (\x -> x == 5) (5: 5 : 5 : Nil)
  log $ show $ zip (1 : 2 : 3 : Nil) (1 : 2 : 3 : 4 : 5 : Nil)
  log $ show $ unzip ((Tuple 1 1) : (Tuple 2 2) : ( Tuple 3 3) : Nil)
  log $ show $ filter (\x -> x > 0) (-1 : -1 : 1 : 2 : 0 : 3 : 4 : Nil)
  log $ show $ filterTail (\x -> x > 0) (-1 : -1 : 1 : 2 : 0 : 3 : 4 : Nil)
  log $ show $ take 5 (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : Nil)