module Otus.Normalize.Value (
  Closure (..),
  VTelescope (..),
  VSubstitution (..),
  VPartialSubstitution,
  Value (..),
  Neutral (..),
  neutralApp,
  freshVar,
  vMetaVar,
  mapMetaVarToNormal,
  vPSubstToList,
) where

import Data.List.NonEmpty (NonEmpty, singleton, (<|))
import Otus.Ast
import Otus.Normalize.Env

data Closure = Closure Environment Term
  deriving (Eq, Show)

newtype VTelescope = VTele [Value]
  deriving (Eq, Show)

newtype VSubstitution = VSubst [Value]
  deriving (Eq, Show)

newtype VPartialSubstitution = VPSubst [Maybe Value]
  deriving (Eq, Show)

data Neutral
  = NVar LevelId
  | NApp Neutral (NonEmpty Value)
  | NNatElim Value Value Neutral
  | NJ Value Value Neutral
  | VDBind Value Neutral
  deriving (Eq, Show)

data Value
  = VNeutral Neutral
  | VMetaVar LevelId
  | VPi Value Closure
  | VLam Closure
  | VNat
  | VZero
  | VSucc Value
  | VId Value Value Value
  | VRefl
  | VType Stage Universe
  | -- Object
    VDynamic VTelescope Value
  | VOk VSubstitution Value
  | VTyErr
  | -- Meta
    VLocal VTelescope Value
  | VPartial VPartialSubstitution Value
  deriving (Eq, Show)

neutralApp :: Neutral -> Value -> Neutral
neutralApp n arg = case n of
  NApp h args -> NApp h (arg <| args)
  _ -> NApp n $ singleton arg

freshVar :: (CtxLike e Value) => e -> Value
freshVar env = VNeutral $ NVar $ LevelId $ ctxLength env

vMetaVar :: LevelId -> Value
vMetaVar = VMetaVar

mapMetaVarToNormal :: Value -> Value
mapMetaVarToNormal (VMetaVar lvl) = VNeutral $ NVar lvl
mapMetaVarToNormal val = val

vPSubstToList :: VPartialSubstitution -> [Maybe Value]
vPSubstToList (VPSubst vals) = vals
