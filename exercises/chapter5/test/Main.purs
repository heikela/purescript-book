module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Picture (Point(..), Shape(..), Picture, bounds, showBounds)

circle :: Shape
circle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

rectangle :: Shape
rectangle = Rectangle (Point { x: 10.0, y: 10.0 }) 10.0 10.0

picture :: Picture
picture = [circle, rectangle]

main :: Eff (console :: CONSOLE) Unit
main = log (showBounds (bounds picture))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

combinations :: Int -> Int -> Int
combinations 0 0 = 1
combinations n 0 = combinations (n - 1) 0
combinations n k | k == n = combinations (n - 1) (k - 1)
combinations n k = combinations (n - 1) k + combinations (n - 1) (k - 1)

sameCity {address: {city: c1}} {address: {city: c2}} | c1 == c2 = true
sameCity _ _ = false

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton a _ = a
