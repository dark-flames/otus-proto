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

import Otus.Ast
import Otus.Common
import {-# SOURCE #-} Otus.Normalize.Env

data Closure = Closure Environment Term
  deriving (Eq, Show)

-- telescope
newtype VTelescope = VTele ValueSeq
  deriving (Eq, Show)

instance Sequence VTelescope where
  type Item VTelescope = Value
  fromSeq = VTele
  toSeq (VTele s) = s

-- substitution
newtype VSubstitution = VSubst ValueSeq
  deriving (Eq, Show)

instance Sequence VSubstitution where
  type Item VSubstitution = Value
  fromSeq = VSubst
  toSeq (VSubst s) = s

-- constraint
data VConstraint
  = VTyEq VTelescope Value Value
  | VTmEq VTelescope Value Value Value
  deriving (Eq, Show)

-- meta definition
data VMetaDefinition
  = VMUnsolved
  | VMGuarded Closure (Seq VConstraint)
  | VMSolved Closure
  deriving (Eq, Show)

data VMetaView
  = UnsolvedMeta
  | SolvedMeta Value
  deriving (Eq, Show)

-- signature
newtype VSignature = VSig (Seq VMetaDefinition)
  deriving (Eq, Show)

instance Sequence VSignature where
  type Item VSignature = VMetaDefinition
  fromSeq = VSig
  toSeq (VSig s) = s

-- neutral
data Neutral
  = NVar LevelId
  | NApp Neutral ValueSeq
  | NNatElim Value Value Neutral
  | NDBind Neutral Closure
  | NForce Neutral
  | NForceUnsolved VSignature Closure
  | NAssign Int VSignature Neutral
  | NOpen Neutral Value
  deriving (Eq, Show)

-- value
type ValueSeq = Seq Value

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
