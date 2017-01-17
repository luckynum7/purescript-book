module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.MonadZero (guard)
import Data.Array (filter, length, null, (!!), (..))
import Data.Array.Partial (head, tail)
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafePartial)

-- 4.4
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

-- 4.7
square :: Array Number -> Array Number
square arr = (\x -> x * x) <$> arr

infix 8 filter as <$?>

removeNegative :: Array Number -> Array Number
-- removeNegative arr = filter (\n -> n > 0.0) arr
removeNegative arr = (\n -> n > 0.0) <$?> arr

-- 4.11
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

intersection :: forall a. Array a -> Array a -> Array (Array (Maybe a))
intersection a b =
  let la = (length a) - 1
      lb = (length b) - 1
  in
   do
     i <- 0 .. la
     j <- 0 .. lb
     pure [a !! i, b !! j]

triples :: Int -> Array (Array Int)
triples n =
  let ln = n - 1
  in
   do
     a <- 1 .. ln
     b <- a .. ln
     c <- b .. ln
     guard $ a * a + b * b == c * c -- && a == b
     pure [a,b,c]

-- TODO: make this work as expected
factorizations :: Int -> Array Int
factorizations 1 = [1]
factorizations n =
   [1, n]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do

  -- 4.11
  logShow $ factorizations 100

  logShow $ triples 100

  logShow $ intersection (1 .. 3) (10 .. 11)
  logShow $ intersection ["a","b"] ["c","d","e"]

  logShow $ isPrime 773
  -- logShow $ isPrime 22338618 -- DO NOT TRY

  -- 4.7
  logShow $ removeNegative $ [0.0, -0.5, 5.0, 3.7, -2.4, 3.0]

  logShow $ square $ [0.0, 1.0, 3.0, 3.5, 7.0, 7.3]

  -- 4.4
  logShow $ countEven $ 1 .. 99
  logShow $ countEven $ 1 .. 100

  logShow $ isEven 99
  logShow $ isEven 100
