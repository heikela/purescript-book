module Main where

import Prelude ((+), (*), Unit)
import Control.Monad.Eff.Console (logShow, CONSOLE)
import Control.Monad.Eff (Eff)
import Math (sqrt, pi)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r

main :: Eff (console :: CONSOLE) Unit
main = logShow (diagonal 3.0 4.0)
