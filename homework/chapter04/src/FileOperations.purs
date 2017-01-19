module FileOperations where

import Prelude
import Control.MonadZero (guard)
import Data.Array (concatMap, filter, foldl, (:))
import Data.Maybe (Maybe(..), Maybe(..), fromMaybe, isJust)
import Data.Path (Path, isDirectory, ls, size)
import Data.Tuple (Tuple(..))

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles file = filter (\x -> not isDirectory x) $ allFiles file

minMax :: Path -> Tuple Int Int
minMax file =
  foldl minMax' (Tuple 0 0)
              (map (\x -> size x) $ onlyFiles file)
  where
    minMax' :: Tuple Int Int -> Maybe Int -> Tuple Int Int
    minMax' tuple Nothing = tuple
    minMax' (Tuple 0 0) (Just z) = Tuple z z
    minMax' (Tuple x y) (Just z) = Tuple (min x z) (max y z)
