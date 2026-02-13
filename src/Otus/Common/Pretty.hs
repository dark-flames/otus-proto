module Otus.Common.Pretty (
  Pretty (..),
) where

class Pretty tm where
  pretty :: tm -> String
