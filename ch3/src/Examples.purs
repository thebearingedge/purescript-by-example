module Examples where

import Prelude

add :: Int -> Int -> Int
add x y = x + y

example :: Int -> Int -> Int -> Int
example x y z = foo + bar
  where
    foo = x * y
    bar = y * z
