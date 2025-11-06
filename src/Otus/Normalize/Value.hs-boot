module Otus.Normalize.Value (
  Value,
  VPartialSubstitution,
  vMetaVar,
  freshVar,
  mapMetaVarToNormal,
  vPSubstToList,
) where

import Otus.Ast

data Value

data VPartialSubstitution

instance Show Value

instance Eq Value

instance Show VPartialSubstitution

instance Eq VPartialSubstitution

vMetaVar :: LevelId -> Value
freshVar :: (CtxLike e Value) => e -> Value
mapMetaVarToNormal :: Value -> Value
vPSubstToList :: VPartialSubstitution -> [Maybe Value]
