module Otus.Normalize.Value (
  Closure (..),
  VTelescope (..),
  VSubstitution (..),
  VConstraint (..),
  VGuardedSubstSeg (.., UnsolvedMeta, SolvedMeta),
  VGuardedSubstitution (..),
  Value (..),
  Neutral (..),
  neutralApp,
  freshVar,
  vVar,
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

data VConstraint
  = VTyEq VTelescope Neutral Neutral
  | VTmEq VTelescope Neutral Neutral
  deriving (Eq, Show)

data VGuardedSubstSeg
  = VUnsolved
  | VSolved Value [VConstraint]
  deriving (Eq, Show)

metaView :: VGuardedSubstSeg -> Maybe Value
metaView = \case
  VUnsolved -> Nothing
  VSolved val [] -> Just val
  VSolved _ (_ : _) -> Nothing

pattern UnsolvedMeta :: VGuardedSubstSeg
pattern UnsolvedMeta <- (metaView -> Nothing)

pattern SolvedMeta :: Value -> VGuardedSubstSeg
pattern SolvedMeta val <- (metaView -> Just val)

{-# COMPLETE UnsolvedMeta, SolvedMeta #-}

newtype VGuardedSubstitution = VGSubst [VGuardedSubstSeg]
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
  | VPi Value Closure
  | VLam Closure
  | VNat Stage
  | VZero Stage
  | VSucc Value
  | VId Value Value Value
  | VRefl
  | VType Stage Universe
  | -- Object
    VForce Value
  | VDynamic VTelescope Value
  | VOk VSubstitution Value
  | VTyErr
  | -- Meta
    VLift Value
  | VQuote Value
  | VLocal VTelescope Value
  | VGuarded VGuardedSubstitution Value
  | VError
  deriving (Eq, Show)

instance Semigroup VTelescope where
  (VTele l) <> (VTele r) = VTele (l ++ r)

instance Semigroup VSubstitution where
  (VSubst l) <> (VSubst r) = VSubst (l ++ r)

instance Semigroup VGuardedSubstitution where
  (VGSubst l) <> (VGSubst r) = VGSubst (l ++ r)

neutralApp :: Neutral -> Value -> Neutral
neutralApp n arg = case n of
  NApp h args -> NApp h (arg <| args)
  _ -> NApp n $ singleton arg

freshVar :: (CtxLike e Value) => e -> Value
freshVar env = VNeutral $ NVar $ LevelId $ ctxLength env

vVar :: LevelId -> Value
vVar = VNeutral . NVar
