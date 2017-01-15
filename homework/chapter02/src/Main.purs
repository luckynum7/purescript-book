module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Math (pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = 2.0 * pi * r

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = logShow (diagonal 3.0 4.0)
