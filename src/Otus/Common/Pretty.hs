module Otus.Common.Pretty (
  Pretty (..),
) where

import Data.Sequence (Seq)

class Pretty tm where
  pretty :: tm -> String

instance (Pretty p) => Pretty (Seq p) where
  pretty s =
    if null s then
      ""
    else
      foldl1 (\p c -> p ++ ", " ++ c) (fmap pretty s)
