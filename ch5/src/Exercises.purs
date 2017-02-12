module Exercises where

import Prelude
import Examples (Person, Point(..), Shape(..), origin)
import Data.Maybe(Maybe(..))
import Global (infinity)
import Math (pi, pow)

factorial :: Int -> Int
factorial n
  | n == 0    = 1
  | otherwise = n * factorial (n - 1)

factorialN :: Number -> Number
factorialN n
  | n == 0.0 = 1.0
  | otherwise = n * factorialN (n - 1.0)

biCo :: Number -> Number -> Number
biCo 0.0 r = 0.0
biCo n 0.0 = infinity
biCo n r = (factorialN n) / (factorialN (n - r) * factorialN r)

biCo' :: Int -> Int -> Int
biCo' _ r | r == 0 = 1
biCo' n r | r > n    = 0
biCo' n r = (biCo' (n - 1) (r - 1)) + (biCo' (n - 1) (r))

tim :: Person
tim =
  { name: "Tim"
  , address : { street: "1234"
              , city:   "Costa Mesa" }
  }

p48 :: Person
p48 =
  { name: "Phantom48"
  , address: { street: "3303"
             , city:   "Costa Mesa" }
  }

jrr :: Person
jrr =
  { name: "JRR"
  , address: { street: "10925"
             , city:   "Fountain Valley" }
  }

sameCity :: forall r a c. Eq c => { address :: { city :: c | a } | r } -> { address :: { city :: c | a } | r } -> Boolean
sameCity { address: { city: x } } { address: { city: y } } = x == y

livesInLA :: forall r a. { address :: { city :: String | a } | r } -> Boolean
livesInLA { address: { city: c } } = c == "Los Angeles"

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton x _   = x

circle :: Shape
circle = Circle origin 10.0

scale :: Shape -> Shape
scale text@(Text _ _)              = text
scale (Clipped pic bounds)         = Clipped (map scale pic) bounds
scale (Circle o r)                 = Circle o (r * 2.0)
scale (Rectangle o x y)            = Rectangle o (x * 2.0) (y * 2.0)
scale (Line (Point p1) (Point p2)) = Line (Point p1') (Point (p2'))
  where
    diffX = (p2.x - p1.x) / 2.0
    diffY = (p2.y - p1.y) / 2.0
    p1'   = { x: p1.x - diffX, y: p1.y - diffY }
    p2'   = { x: p2.x + diffX, y: p2.y + diffY }

getText :: Shape -> Maybe String
getText (Text _ t) = Just t
getText _          = Nothing

area :: Shape -> Number
area (Circle _ r)      = pi * (pow r 2.0)
area (Rectangle _ w h) = w * h
area _                 = 0.0
