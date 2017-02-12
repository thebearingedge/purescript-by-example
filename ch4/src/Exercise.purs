module Exercise where

import Prelude
import Control.MonadZero (guard)
import Data.Array ((:), (..), null, filter, length)
import Data.Array.Partial (head, tail)
import Data.Foldable (product, foldl)
import Example (factors)
import Partial.Unsafe (unsafePartial)

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

countEvens :: Array Int -> Int
countEvens arr =
  if null arr
    then 0
    else if isEven (unsafePartial head arr)
           then 1 + countEvens (unsafePartial tail arr)
           else countEvens (unsafePartial tail arr)

toSquares :: Array Int -> Array Int
toSquares ns = (\n -> n * n) <$> ns

infix 8 filter as <$?>

aboveZero :: Array Int -> Array Int
aboveZero ns = (\n -> n >= 0) <$?> ns

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

crossJoin :: Array Int -> Array Int -> Array (Array Int)
crossJoin xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. m
  b <- 1 .. m
  c <- 1 .. m
  guard $ a * a + b * b == c * c
  pure [a, b, c]
  where m = n - 1

factorizations :: Int -> Array (Array Int)
factorizations n
  | n == 1    = [[1]]
  | otherwise = filter (produces n) $ map (expand n) (factors n)
  where
    factors x = do
      f <- x .. 2
      guard $ x `mod` f == 0
      pure f
    expand x f
      | f `div` x == 1 = [f, 1]
      | f * f > x      = [f, x `div` f, 1]
      | otherwise      = f : expand (x `div` f) f
    produces x = \fs -> product fs == x

allTrue :: Array Boolean -> Boolean
allTrue xs = foldl (\acc x -> acc && x) true xs

countR :: forall a. (a -> Boolean) -> Array a -> Int
countR _ [] = 0
countR p xs = if p (unsafePartial head xs)
                then countR p (unsafePartial tail xs) + 1
                else countR p (unsafePartial tail xs)

countT :: forall a. (a -> Boolean) -> Array a -> Int
countT p = count 0
  where
    count acc [] = acc
    count acc xs = if (p $ unsafePartial head xs)
                      then count (acc + 1) (unsafePartial tail xs)
                      else count acc (unsafePartial tail xs)

reverse' :: forall a. Array a -> Array a
reverse' xs = foldl (\rs x -> x : rs) [] xs
