module Otus.Normalize.Value (
  Value,
  vMetaVar,
) where

import Otus.Ast

data Value

instance Show Value

instance Eq Value

vMetaVar :: LevelId -> Value
