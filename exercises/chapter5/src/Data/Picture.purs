module Data.Picture where

import Prelude

import Data.Foldable (foldl)
import Data.Either (Either(..))
import Global as Global
import Math as Math

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture Bounds

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
showShape (Clipped _ _) = "Clipped picture"

instance showShapeInstance :: Show Shape where
  show = showShape

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped pic clipBounds) = intersect (bounds pic) clipBounds

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

area :: Shape -> Either String Number
area (Circle _ r) = Right $ Math.pi * r * r
area (Line _ _) = Right 0.0
area (Text _ _) = Right 0.0
area (Rectangle _ w h) = Right $ w * h
area (Clipped _ _) = Left "Computing the area of a clipped picture not implemented"
