module Otus.Normalize.Value (
  Value,
  VTelescope (..),
  vMetaVar,
  freshVar,
  mapMetaVarToNormal,
) where

import Otus.Ast

data Value

instance Show Value

instance Eq Value

newtype VTelescope = VTele [Value]

instance Show VTelescope

instance Eq VTelescope

vMetaVar :: LevelId -> Value
freshVar :: (CtxLike e Value) => e -> Value
mapMetaVarToNormal :: Value -> Value
