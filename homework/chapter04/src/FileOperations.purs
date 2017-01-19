module FileOperations where

import Prelude
import Data.Array (concatMap, filter, (:))
import Data.Path (Path, isDirectory, ls)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles file = filter (\x -> not isDirectory x) $ allFiles file
