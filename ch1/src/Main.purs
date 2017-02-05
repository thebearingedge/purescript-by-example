module Main where

import Prelude
import Control.Monad.Eff.Console (log)

iAmANumber =
  let square x = x * x
   in square 42.0

iterate f 0 x = x
iterate f n x = iterate f (n - 1) (f x)

main = log "Hello, World!"
