module Exercises where

import Data.Array ((:))
import Data.BooleanAlgebra ((&&))
import Data.Eq (class Eq, (==))
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Functor (class Functor, map)
import Data.Monoid (class Monoid)
import Data.Monoid.Additive (Additive(..))
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Semigroup (class Semigroup, (<>))
import Data.Show (class Show, show)
import Prelude ((-))

instance showShape :: Show Shape where
  show (Circle c r) =
    "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
  show (Rectangle c w h) =
    "Rectangle [center: " <> showPoint c <>
    ", width: " <> show w <> ", height: " <> show h <> "]"
  show (Line p1 p2) =
    "Line [start: " <> showPoint p1 <> ", end: " <> showPoint p2 <> "]"
  show (Text c t) =
    "Text [center: " <> showPoint c <> " '" <> t <> "']"
  show (Clipped p _) = "Clipped Picture: " <> show (map show p)

type Picture = Array Shape

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture Shape

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex = Complex
  { real      :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real: a, imaginary: b }) = show a <> " + " <> show b <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex n1) (Complex n2) = n1.real == n2.real &&
                                 n1.imaginary == n2.imaginary

data NonEmpty x = NonEmpty x (Array x)

instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a)
  where
    eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

instance semigroupNonEmpty :: (Semigroup a, Semigroup (Array a)) => Semigroup (NonEmpty a)
  where
    append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> (y : ys))

instance functorNonEmpty :: Functor NonEmpty
  where
    map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

instance foldableNonEmpty :: Foldable NonEmpty
  where
    foldl f z (NonEmpty x xs) = foldl f z (x : xs)
    foldr f z (NonEmpty x xs) = foldr f z (x : xs)
    foldMap f (NonEmpty x xs) = foldMap f (x : xs)

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a)
  where
    eq (Finite x) (Finite y) = x == y
    eq Infinite   Infinite   = true
    eq _          _          = false

instance ordExtended :: Ord a => Ord (Extended a)
  where
    compare (Finite x) (Finite y) = compare x y
    compare Infinite   (Finite _) = GT
    compare (Finite _) (Infinite) = LT
    compare _          _          = EQ

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f)
  where
    foldl f x (OneMore y xs) = foldl f (f x y) xs
    foldr f x (OneMore y xs) = f y (foldr f x xs)
    foldMap f (OneMore y xs) = (f y) <> (foldMap f xs)

class Monoid m <= Action m a
  where
    act :: m -> a -> a

instance repeatAction :: Action (Additive Int) String
  where
    act (Additive t) s = repeat t s
      where
        repeat 0 _ = ""
        repeat times string = string <> repeat (times - 1) string

instance eachAction :: Action m a => Action m (Array a)
  where
    act x ys = map (act x) ys

newtype Self m = Self m

instance selfAction :: Monoid m => Action m (Self m)
  where
    act _ (Self x) = Self (x <> x)
