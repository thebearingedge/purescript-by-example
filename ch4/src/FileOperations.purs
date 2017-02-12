module FileOperations where

import Prelude
import Control.MonadZero (guard)
import Data.Array (concatMap, foldl, head, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Path (Path, isDirectory, ls, root, size, filename)
import Data.Tuple (Tuple(..), fst, snd)

allFiles :: Path -> Array Path
allFiles p = p : concatMap allFiles (ls p)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles p
  | isDirectory p = (concatMap onlyFiles) (ls p)
  | otherwise     = p : []

onlyFiles' :: Path -> Array Path
onlyFiles' p = do
  dir <- ls p
  guard $ isDirectory p
  file <- onlyFiles dir
  pure file

sizeRange :: Tuple (Maybe Int) (Maybe Int)
sizeRange = foldl replace (Tuple firstSize Nothing) files
  where
    files     = onlyFiles root
    firstSize = maybe Nothing size $ head files
    replace   = (\sizes file -> Tuple (min (size file) (fst sizes)) (max (size file) (snd sizes)))

whereIs :: String -> Maybe Path
whereIs p = head found
  where
    found = do
      dir <- allFiles root
      guard $ isDirectory dir
      file <- ls dir
      guard $ filename file == p
      pure dir
