module Otus.Normalize.Object.Value (
  ObjEnv,
  ObjClosure (..),
  VTelescope (..),
  VRecord (..),
  VConstraint (..),
  VProblem (..),
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
newtype VRecord = VRecord ObjValueSeq
  deriving (Eq, Show)

instance SeqSize VRecord where
  size (VRecord s) = size s

instance Sequence VRecord where
  type Item VRecord = ObjValue
  fromSeq = VRecord
  toSeq (VRecord s) = s

-- constraint
data VConstraint
  = VTmEq Int ObjValue ObjValue
  deriving (Eq, Show)

-- signature
newtype VProblem = VProb (Seq VConstraint)
  deriving (Eq, Show)

instance SeqSize VProblem where
  size (VProb s) = size s

instance Sequence VProblem where
  type Item VProblem = VConstraint
  fromSeq = VProb
  toSeq (VProb s) = s

-- neutral
data ObjNeutral
  = ONFlex LevelId ObjValueSeq
  | ONRigid LevelId ObjValueSeq
  deriving (Eq, Show)

-- value
type ObjValueSeq = Seq ObjValue

data ObjValue
  = OVNeutral ObjNeutral
  | OVPi ObjValue ObjClosure
  | OVLam ObjClosure
  | OVType
  deriving (Eq, Show)

instance Value ObjValue where
  type Neutral ObjValue = ObjNeutral
  vVar lvl = OVNeutral $ ONFlex lvl empty
  fromNeutral = OVNeutral

objNeutralApp :: ObjNeutral -> ObjValue -> ObjNeutral
objNeutralApp n arg = case n of
  ONFlex h args -> ONFlex h (args |> arg)
  ONRigid h args -> ONRigid h (args |> arg)
