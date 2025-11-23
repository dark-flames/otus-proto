module Otus.Normalize.Value (
  Closure (..),
  VTelescope (..),
  VSubstitution (..),
  VConstraint (..),
  VMetaDefinition (..),
  VMetaView (..),
  VSignature (..),
  Value (..),
  Neutral (..),
  vVar,
  neutralApp,
) where

import Data.List.NonEmpty (NonEmpty, singleton, (<|))

import Otus.Ast
import Otus.Common
import {-# SOURCE #-} Otus.Normalize.Env

data Closure = Closure Environment Term
  deriving (Eq, Show)

-- telescope
newtype VTelescope = VTele [Value]
  deriving (Eq, Show)

instance Semigroup VTelescope where
  (VTele l) <> (VTele r) = VTele (l ++ r)

instance Sized VTelescope where
  size (VTele l) = length l

-- substitution
newtype VSubstitution = VSubst [Value]
  deriving (Eq, Show)

instance Semigroup VSubstitution where
  (VSubst l) <> (VSubst r) = VSubst (l ++ r)

instance Sized VSubstitution where
  size (VSubst l) = length l

-- constraint
data VConstraint
  = VTyEq VTelescope Value Value
  | VTmEq VTelescope Value Value
  deriving (Eq, Show)

-- meta definition
data VMetaDefinition
  = VMUnsolved
  | VMGuarded Closure [VConstraint]
  | VMSolved Closure
  deriving (Eq, Show)

data VMetaView
  = UnsolvedMeta
  | SolvedMeta Value
  deriving (Eq, Show)

-- signature
newtype VSignature = VSig [VMetaDefinition]
  deriving (Eq, Show)

instance Semigroup VSignature where
  (VSig l) <> (VSig r) = VSig (l ++ r)

instance Sized VSignature where
  size (VSig l) = length l

-- neutral
data Neutral
  = NVar LevelId
  | NApp Neutral (NonEmpty Value)
  | NNatElim Value Value Neutral
  | NDBind Closure Neutral
  | NForce Neutral
  deriving (Eq, Show)

-- value
data Value
  = VNeutral Neutral
  | VPi Value Closure
  | VLam Closure
  | VNat Stage
  | VZero Stage
  | VSucc Value
  | VType Stage Universe
  | -- Object
    VDynamic VTelescope Value
  | VOk VSubstitution Value
  | VTyErr
  | -- Meta
    VLocal VTelescope Value
  | VGuarded VSignature Closure
  | VError
  deriving (Eq, Show)

vVar :: LevelId -> Value
vVar = VNeutral . NVar

neutralApp :: Neutral -> Value -> Neutral
neutralApp n arg = case n of
  NApp h args -> NApp h (arg <| args)
  _ -> NApp n $ singleton arg
