module FileOperations where

import Prelude

import Data.Path (Path, ls)
import Data.Array (concatMap, (:))

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
