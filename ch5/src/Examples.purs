module Examples where

import Data.Array.Partial (tail)
import Data.Foldable (sum, foldl)
import Data.Functor (map)
import Data.Ord (class Ord, (<=))
import Global (infinity)
import Math (min, max)
import Partial.Unsafe (unsafePartial)
import Prelude ((>), (-), (+), (*), (/), (<>), otherwise, show, negate)

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
            then gcd (n - m) m
            else gcd n (m - n)

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m | n > m     = gcd' (n - m) m
         | otherwise = gcd' n (m - n)

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _  = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _               = 0

showPerson :: { first :: String, last :: String } -> String
showPerson { first: f, last: l } = l <> ", " <> f

type Address =
  { street :: String
  , city   :: String
  }

type Person =
  { name    :: String
  , address :: Address
  }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _                                    = false

sortPair :: forall a. (Ord a) => Array a -> Array a
sortPair arr@[x, y]
  | x <= y    = arr
  | otherwise = [y, x]
sortPair arr = arr

lzs :: Array Int -> Array Int
lzs [] = []
lzs xs = case sum xs of
           0 -> xs
           _ -> unsafePartial tail xs

partialFunction :: Boolean -> Boolean
partialFunction = unsafePartial (\true -> true)

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

data Maybe a = Nothing | Just a

data List a = Nil | Cons a (List a)

exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point { x: 0.0, y: 2.0 }
    p2 :: Point
    p2 = Point { x: 2.0, y: 0.0 }

showPoint :: Point -> String
showPoint (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line p1 p2) =
  "Line [start: " <> showPoint p1 <> ", end: " <> showPoint p2 <> "]"
showShape (Text c t) =
  "Text [center: " <> showPoint c <> " '" <> t <> "']"
showShape (Clipped p _) = "Clipped Picture: " <> show (map showShape p)

origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0

newtype Pixels = Pixels Number
newtype Inches = Inches Number

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:    infinity
  , left:   infinity
  , bottom: -infinity
  , right:  -infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -infinity
  , left:   -infinity
  , bottom: infinity
  , right:  infinity
  }

showBounds :: Bounds -> String
showBounds (Bounds { top, right, bottom, left }) =
  "Bounds [top: " <> show top <>
  ", left: " <> show left <>
  ", bottom: " <> show bottom <>
  ", right: " <> show right <> "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    min p1.y p2.y
  , left:   min p1.x p2.x
  , bottom: max p1.y p2.y
  , right:  max p1.x p2.x
  }
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Clipped p shape) = shapeBounds shape

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    min b1.top    b2.top
  , left:   min b1.left   b2.left
  , bottom: max b1.bottom b2.bottom
  , right:  max b1.right  b2.right
  }

infixl 4 union as \/

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    max b1.top    b2.top
  , left:   max b1.left   b2.left
  , bottom: min b1.bottom b2.bottom
  , right:  min b1.right  b2.right
  }

infixl 4 intersect as /\

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = shapeBounds shape \/ b
