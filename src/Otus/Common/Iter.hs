module Otus.Common.Iter (
  Sized (..),
) where

import Data.Foldable as F

class Sized a where
  size :: a -> Int

instance (Foldable t) => Sized (t a) where
  size = F.length
