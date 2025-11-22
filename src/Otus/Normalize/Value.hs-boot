module Otus.Normalize.Value (
  Value,
  VTelescope (..),
  VConstraint,
  VMetaDefinition (..),
  vVar,
  freshVar,
  maybeVar,
  metaView,
) where

import Otus.Ast

newtype VTelescope = VTele [Value]

instance Show VTelescope

instance Eq VTelescope

data VConstraint

instance Show VConstraint

instance Eq VConstraint

data VMetaDefinition
  = VUnsolved
  | VSolved Value [VConstraint]

instance Show VMetaDefinition

instance Eq VMetaDefinition

data Value

instance Show Value

instance Eq Value

vVar :: LevelId -> Value
freshVar :: (CtxLike e Value) => e -> Value
maybeVar :: Value -> Maybe LevelId
metaView :: VMetaDefinition -> Maybe Value
