module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Examples (
    Point(..)
  , Shape(..)
  , showShape
  , shapeBounds
  , showBounds
)

circ :: Shape
circ = Circle (Point { x: 0.0, y: 0.0 }) 1.0

rect :: Shape
rect = Rectangle (Point { x: 1.0, y: 2.0 }) 3.0 4.0

line :: Shape
line = Line (Point { x: 2.0, y: 2.0 }) (Point { x: 4.0, y: 4.0 })

clip :: Shape
clip = Clipped [circ, rect, line] rect

shapeName :: Shape -> String
shapeName (Circle _ _)      = "Circle"
shapeName (Rectangle _ _ _) = "Rectangle"
shapeName (Line _ _)        = "Line"
shapeName (Text _ _)        = "Text"
shapeName (Clipped pic _)   = "Clipped: " <> show (map shapeName pic)

printShape :: Shape -> String
printShape shape =
  "\n\n---- " <> shapeName shape <> " ----" <>
  "\n" <> showShape shape <>
  "\n" <> (showBounds $ shapeBounds shape)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ printShape circ
  log $ printShape rect
  log $ printShape line
  log $ printShape clip
