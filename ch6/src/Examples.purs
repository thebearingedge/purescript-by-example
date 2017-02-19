module Examples where

import Prelude (
  (<>), (==), (<), (&&),
  otherwise,
  class Eq, class Ord, class Functor
)
import Data.Tuple (Tuple(..))
import Data.Monoid (append, mempty)
import Data.Foldable (foldl)

class Show a where
  show :: a -> String

instance showBoolean :: Show Boolean where
  show true  = "true"
  show false = "false"

instance showTuple :: Show (Tuple Boolean Boolean) where
  show (Tuple a b) = "(" <> show a <> ", " <> show b <> ")"

data Ordering = LT | EQ | GT

appended :: String
appended = foldl append mempty ["Hello", " ", "World"]

threeAreEqual :: forall a. Eq a => a -> a -> a -> Boolean
threeAreEqual x y z = x == y && y == z

showCompare :: forall a. (Show a, Eq a, Ord a) => a -> a -> String
showCompare a b
  | a < b     = show a <> " is less than "    <> show b
  | a == b    = show a <> " is equal to "     <> show b
  | otherwise = show a <> " is greater than " <> show b

data Maybe a = Just a | Nothing

instance functorMaybe :: Functor Maybe
  where
    map f (Just a) = Just (f a)
    map _ _        = Nothing
