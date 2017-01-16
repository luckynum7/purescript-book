module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Array (filter, null, (..))
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

square :: Array Number -> Array Number
square arr = (\x -> x * x) <$> arr

infix 8 filter as <$?>

removeNegative :: Array Number -> Array Number
-- removeNegative arr = filter (\n -> n > 0.0) arr
removeNegative arr = (\n -> n > 0.0) <$?> arr


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ isEven 99
  logShow $ isEven 100

  logShow $ countEven $ 1 .. 99
  logShow $ countEven $ 1 .. 100

  logShow $ square $ [1.0, 3.0, 3.5, 7.0, 7.3]

  logShow $ removeNegative $ [-0.5, 5.0, 3.7, -2.4, 3.0]
