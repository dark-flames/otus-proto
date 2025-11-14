module Otus.Normalize.Value (
  Closure (..),
  VTelescope (..),
  VSubstitution (..),
  Value (..),
  Neutral (..),
  neutralApp,
  freshVar,
  vMetaVar,
  mapMetaVarToNormal,
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

data Neutral
  = NVar LevelId
  | NApp Neutral (NonEmpty Value)
  | NNatElim Value Value Neutral
  | NJ Value Value Neutral
  | VDBind Closure Neutral
  | VOpen Closure Neutral
  deriving (Eq, Show)

data Value
  = VNeutral Neutral
  | VMetaVar LevelId
  | VPi Value Closure
  | VLam Closure
  | VNat Stage
  | VZero Stage
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
  | VPartial VTelescope VSubstitution Value
  | VError
  deriving (Eq, Show)

instance Semigroup VTelescope where
  (VTele l) <> (VTele r) = VTele (l ++ r)

instance Semigroup VSubstitution where
  (VSubst l) <> (VSubst r) = VSubst (l ++ r)

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
