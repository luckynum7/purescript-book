module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x = isEven $ x - 2

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ isEven 99
  logShow $ isEven 100
