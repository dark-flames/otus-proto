module Otus.Normalize.Value (
  Closure (..),
  VTelescope (..),
  VSubstitution (..),
  VConstraint (..),
  VMetaDefinition (..),
  VMetaView (..),
  VSignature (..),
  Value (..),
  ValueSeq,
  Neutral (..),
  vVar,
  neutralApp,
) where

import Data.List.NonEmpty (NonEmpty, singleton, (<|))

import qualified Data.Sequence as Seq

import Otus.Ast
import Otus.Common
import {-# SOURCE #-} Otus.Normalize.Env

data Closure = Closure Environment Term
  deriving (Eq, Show)

-- telescope
newtype VTelescope = VTele ValueSeq
  deriving (Eq, Show)

instance Semigroup VTelescope where
  (VTele l) <> (VTele r) = VTele (l Seq.>< r)

instance Sized VTelescope where
  size (VTele l) = length l

-- substitution
newtype VSubstitution = VSubst ValueSeq
  deriving (Eq, Show)

instance Semigroup VSubstitution where
  (VSubst l) <> (VSubst r) = VSubst (l Seq.>< r)

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
  | VMGuarded Closure (Seq.Seq VConstraint)
  | VMSolved Closure
  deriving (Eq, Show)

data VMetaView
  = UnsolvedMeta
  | SolvedMeta Value
  deriving (Eq, Show)

-- signature
newtype VSignature = VSig (Seq.Seq VMetaDefinition)
  deriving (Eq, Show)

instance Semigroup VSignature where
  (VSig l) <> (VSig r) = VSig (l Seq.>< r)

instance Sized VSignature where
  size (VSig l) = length l

-- neutral
data Neutral
  = NVar LevelId
  | NApp Neutral (NonEmpty Value)
  | NNatElim Value Value Neutral
  | NDBind Neutral Closure
  | NForce Neutral
  | NAssign Int VSignature Neutral
  | NOpen Neutral Value
  deriving (Eq, Show)

-- value
type ValueSeq = Seq.Seq Value

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
    VLocal VTelescope VTelescope Value
  | VGuarded Int VSignature Closure
  | VError
  deriving (Eq, Show)

vVar :: LevelId -> Value
vVar = VNeutral . NVar

neutralApp :: Neutral -> Value -> Neutral
neutralApp n arg = case n of
  NApp h args -> NApp h (arg <| args)
  _ -> NApp n $ singleton arg
