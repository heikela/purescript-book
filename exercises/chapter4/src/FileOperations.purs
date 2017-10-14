module FileOperations where

import Prelude

import Data.Path (Path, ls)
import Data.Array (concatMap, filter, (:), null)
import Data.Array.Partial (head, tail)
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
