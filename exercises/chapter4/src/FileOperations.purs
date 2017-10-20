module FileOperations where

import Prelude

import Data.Path (Path, ls)
import Data.Array (concatMap, filter, (:), (..), null)
import Data.Array.Partial (head, tail)
import Data.Foldable
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

evenInteger :: Int -> Boolean
evenInteger 0 = true
evenInteger n =
  not if n < 0
    then evenInteger $ n + 1
    else evenInteger $ n - 1

countEvenInts :: Array Int -> Int
countEvenInts arr =
  if null arr
    then 0
    else if evenInteger headElem
      then 1 + countEvenInts tailArr
      else countEvenInts tailArr
    where
      headElem = unsafePartial head arr
      tailArr = unsafePartial tail arr

infixr 9 filter as <$?>

squares :: Array Int -> Array Int
squares = map \x -> x * x

nonNegatives :: Array Int -> Array Int
nonNegatives arr = (\x -> if x < 0 then false else true) <$?> arr

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1..n
  j <- i..n
  guard $ i * j == n
  pure [i, j]

cartesian :: forall a. Array a -> Array a -> Array (Array a)
cartesian arr1 arr2 = do
  i <- arr1
  j <- arr2
  pure [i, j]

pythagoreanTriples :: Int -> Array (Array Int)
pythagoreanTriples n = do
  a <- 1..n
  b <- a..n
  c <- b..n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

factorizations :: Int -> Array (Array Int)
factorizations 1 = [[1]]
factorizations n = do
  a <- 2..n
  b <- 1..n
  guard $ a * b == n
  factorizationsOfB <- factorizations b
  pure $ a : factorizationsOfB

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true
