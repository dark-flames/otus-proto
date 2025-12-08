module Otus.Normalize.Object.Value (
  ObjEnv,
  ObjClosure (..),
  VTelescope (..),
  VSubstitution (..),
  VConstraint (..),
  VMetaDefinition (..),
  VSignature (..),
  ObjValue (..),
  ObjValueSeq,
  ObjNeutral (..),
  objNeutralApp,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize.Env

type ObjEnv = Environment ObjValue

data ObjClosure = ObjClosure ObjEnv ObjTerm
  deriving (Eq, Show)

-- telescope
newtype VTelescope = VTele ObjValueSeq
  deriving (Eq, Show)

instance SeqSize VTelescope where
  size (VTele s) = size s

instance Sequence VTelescope where
  type Item VTelescope = ObjValue
  fromSeq = VTele
  toSeq (VTele s) = s

-- substitution
newtype VSubstitution = VSubst ObjValueSeq
  deriving (Eq, Show)

instance SeqSize VSubstitution where
  size (VSubst s) = size s

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
  = VUnsolved
  | VGuarded ObjClosure (Seq VConstraint)
  | VSolved ObjClosure
  deriving (Eq, Show)

-- signature
newtype VSignature = VSig (Seq VMetaDefinition)
  deriving (Eq, Show)

instance SeqSize VSignature where
  size (VSig s) = size s

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
  | OVPi ObjValue ObjClosure
  | OVLam ObjClosure
  | OVType Stage Universe
  deriving (Eq, Show)

instance Value ObjValue where
  type Neutral ObjValue = ObjNeutral
  vVar lvl = OVNeutral $ ONVar lvl
  fromNeutral = OVNeutral

objNeutralApp :: ObjNeutral -> ObjValue -> ObjNeutral
objNeutralApp n arg = case n of
  ONApp h args -> ONApp h (arg <| args)
  _ -> ONApp n $ singleton arg
