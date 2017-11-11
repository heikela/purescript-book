module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Picture (Point(..), Shape(..), Picture, Bounds(..), bounds, showBounds)
import Data.Maybe

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

multiply :: Point -> Number -> Point
multiply (Point {x, y}) multiplier = Point {x: x * multiplier, y: y * multiplier}

scaleBounds :: Bounds -> Number -> Bounds
scaleBounds (Bounds {top, left, bottom, right}) mul = Bounds
  {
    top: top * mul,
    left: left * mul,
    bottom: bottom * mul,
    right: right * mul
  }

doubleSize :: Shape -> Shape
doubleSize (Circle point size) = Circle (multiply point 2.0) (size * 2.0)
doubleSize (Rectangle topLeft w h) = Rectangle (multiply topLeft 2.0) (2.0 * w) (2.0 * h)
doubleSize (Line p1 p2) = Line (multiply p1 2.0) (multiply p2 2.0)
doubleSize (Text loc txt) = Text (multiply loc 2.0) txt
doubleSize (Clipped picture bounds) = Clipped (map doubleSize picture) (scaleBounds bounds 2.0)

shapeText :: Shape -> Maybe String
shapeText (Text _ txt) = Just txt
shapeText _ = Nothing
