module Otus.Normalize.Value (
  Value,
  VTelescope (..),
  VConstraint,
  VGuardedSubstSeg (..),
  vVar,
  freshVar,
) where

import Otus.Ast

newtype VTelescope = VTele [Value]

instance Show VTelescope

instance Eq VTelescope

data VConstraint

instance Show VConstraint

instance Eq VConstraint

data VGuardedSubstSeg
  = VUnsolved
  | VSolved Value [VConstraint]

instance Show VGuardedSubstSeg

instance Eq VGuardedSubstSeg

data Value

instance Show Value

instance Eq Value

vVar :: LevelId -> Value
freshVar :: (CtxLike e Value) => e -> Value
