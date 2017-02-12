module Example where

import Prelude
import Control.MonadZero (guard)
import Data.Array ((..), null, filter, concatMap)
import Data.Array.Partial (tail)
import Data.Foldable (product)
import Partial.Unsafe (unsafePartial)

fact :: Int -> Int
fact n
  | n < 2     = 1
  | otherwise = n * fact (n - 1)

fib :: Int -> Int
fib n
  | n == 0    = 1
  | n == 1    = 1
  | otherwise = fib (n - 1) + fib (n - 2)

length' :: forall a. Array a -> Int
length' array =
  if null array
    then 0
    else 1 + length' (unsafePartial tail array)

evens :: Array Int
evens = (\x -> x * 2) <$> [1, 2, 3]

evenToTen :: Array Int
evenToTen = filter (\n -> n `mod` 2 == 0) (1 .. 10)

pairs :: Int -> Array (Array Int)
pairs n = concatMap (\x -> map (\y -> [x, y]) (x .. n)) (1 .. n)

factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs n)

factors' :: Int -> Array (Array Int)
factors' n = filter (\pair -> product pair == n) $ do
  x <- 1 .. n
  y <- x .. n
  pure [x, y]

factors'' :: Int -> Array (Array Int)
factors'' n = do
  x <- 1 .. n
  y <- x .. n
  guard $ x * y == n
  pure [x, y]
