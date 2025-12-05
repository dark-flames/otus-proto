module Otus.Normalize.Object.Value (
  ObjEnv,
  Closure (..),
  VTelescope (..),
  VSubstitution (..),
  VConstraint (..),
  VMetaDefinition (..),
  VSignature (..),
  ObjValue (..),
  ObjValueSeq,
  ObjNeutral (..),
  neutralApp,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize.Env

type ObjEnv = Environment ObjValue

data Closure = Closure ObjEnv ObjTerm
  deriving (Eq, Show)

-- telescope
newtype VTelescope = VTele ObjValueSeq
  deriving (Eq, Show)

instance Sequence VTelescope where
  type Item VTelescope = ObjValue
  fromSeq = VTele
  toSeq (VTele s) = s

-- substitution
newtype VSubstitution = VSubst ObjValueSeq
  deriving (Eq, Show)

instance Sequence VSubstitution where
  type Item VSubstitution = ObjValue
  fromSeq = VSubst
  toSeq (VSubst s) = s

-- constraint
data VConstraint
  = VTyEq VTelescope ObjValue ObjValue
  | VTmEq VTelescope ObjValue ObjValue ObjValue
  deriving (Eq, Show)

-- meta definition
data VMetaDefinition
  = VMUnsolved
  | VMGuarded Closure (Seq VConstraint)
  | VMSolved Closure
  deriving (Eq, Show)

-- signature
newtype VSignature = VSig (Seq VMetaDefinition)
  deriving (Eq, Show)

instance Sequence VSignature where
  type Item VSignature = VMetaDefinition
  fromSeq = VSig
  toSeq (VSig s) = s

-- neutral
data ObjNeutral
  = ONVar LevelId
  | ONApp ObjNeutral ObjValueSeq
  deriving (Eq, Show)

-- value
type ObjValueSeq = Seq ObjValue

data ObjValue
  = OVNeutral ObjNeutral
  | OVPi ObjValue Closure
  | OVLam Closure
  | OVType Stage Universe
  deriving (Eq, Show)

instance Value ObjValue where
  type Neutral ObjValue = ObjNeutral
  vVar lvl = OVNeutral $ ONVar lvl
  fromNeutral = OVNeutral

neutralApp :: ObjNeutral -> ObjValue -> ObjNeutral
neutralApp n arg = case n of
  ONApp h args -> ONApp h (arg <| args)
  _ -> ONApp n $ singleton arg
