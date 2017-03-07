module Stream where

import Prelude ((<>))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.String as String

class Stream stream element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons

instance streamString :: Stream String Char where
  uncons = String.uncons

foldStream :: forall l e m. (Stream l e, Monoid m) => (e -> m) -> l -> m
foldStream f stream =
  case uncons stream of
    Nothing             -> mempty
    Just { head, tail } -> f head <> foldStream f tail
