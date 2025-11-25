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

import qualified Data.Sequence as Seq

import Otus.Ast
import Otus.Common
import {-# SOURCE #-} Otus.Normalize.Env

data Closure = Closure Environment Term
  deriving (Eq, Show)

-- telescope
newtype VTelescope = VTele ValueSeq
  deriving (Eq, Show)

instance Sized VTelescope where
  size (VTele l) = length l

instance AppendL VTelescope Value where
  val <| (VTele l) = VTele (val <| l)

instance AppendR VTelescope Value where
  (VTele l) |> val = VTele (l |> val)

instance Extend VTelescope VTelescope where
  (VTele l) >< (VTele r) = VTele (l >< r)

-- substitution
newtype VSubstitution = VSubst ValueSeq
  deriving (Eq, Show)

instance Sized VSubstitution where
  size (VSubst l) = length l

instance AppendL VSubstitution Value where
  val <| (VSubst l) = VSubst (val <| l)

instance AppendR VSubstitution Value where
  (VSubst l) |> val = VSubst (l |> val)

instance Extend VSubstitution VSubstitution where
  (VSubst l) >< (VSubst r) = VSubst (l >< r)

-- constraint
data VConstraint
  = VTyEq VTelescope Value Value
  | VTmEq VTelescope Value Value Value
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

instance Sized VSignature where
  size (VSig l) = length l

instance Semigroup VSignature where
  (VSig l) <> (VSig r) = VSig $ l >< r

instance AppendR VSignature VMetaDefinition where
  (VSig l) |> def = VSig $ l |> def

-- neutral
data Neutral
  = NVar LevelId
  | NApp Neutral ValueSeq
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
  _ -> NApp n $ Seq.singleton arg
