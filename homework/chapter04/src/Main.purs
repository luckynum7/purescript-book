module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Array (null, (..))
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x = isEven $ x - 2

countEven :: Array Int -> Int
countEven arr =
  if null arr
     then 0
     else if isEven $ unsafePartial head arr
             then 1 + countEven (unsafePartial tail arr)
             else countEven (unsafePartial tail arr)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ isEven 99
  logShow $ isEven 100

  logShow $ countEven $ 1 .. 99
  logShow $ countEven $ 1 .. 100
