module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, (:), (..), null)
import Data.Array (head) as ArrOpt
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Path (Path, isDirectory, ls, size, root, filename)
import Partial.Unsafe (unsafePartial)

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

count :: forall a. (a -> Boolean) -> Array a -> Int
count = count' 0
  where
    count' :: Int -> (a -> Boolean) -> Array a -> Int
    count' acc _ [] = acc
    count' acc p xs = if p (unsafePartial head xs)
                        then count' (acc + 1) p (unsafePartial tail xs)
                        else count' acc p (unsafePartial tail xs)

reverse :: forall a. Array a -> Array a
reverse = foldl (\reversedHead next -> next : reversedHead) []

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

largestAndSmallest :: Path -> Maybe {largest:: Path, smallest:: Path}
largestAndSmallest fromPath =
  foldl updateLargestAndSmallest Nothing (onlyFiles fromPath)
    where
      updateLargestAndSmallest :: Maybe {largest:: Path, smallest:: Path} -> Path -> Maybe {largest:: Path, smallest:: Path}
      updateLargestAndSmallest Nothing path = Just {largest: path, smallest: path}
      updateLargestAndSmallest (Just acc) path = Just {
        largest: (if size path > size acc.largest then path else acc.largest),
        smallest: (if size path < size acc.smallest then path else acc.smallest)
      }

findIn :: Path -> String -> Array Path
findIn rootPath nameToFind = do
  subPath <- ls rootPath
  if not isDirectory subPath
    then
      if filename subPath == nameToFind
        then pure rootPath
        else []
    else
      findIn subPath nameToFind


whereIs :: String -> Maybe Path
whereIs = ArrOpt.head <<< findIn root
