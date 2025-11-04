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
  mapMetaVar,
  vPSubstToList,
) where

import Data.List.NonEmpty (NonEmpty, singleton, (<|))
import Otus.Ast
import Otus.Normalize.Env

data Closure = Closure Environment Term
  deriving (Eq, Show)

data VTelescope
  = VTNil
  | VTCons Value VTelescope
  deriving (Eq, Show)

data VSubstitution
  = VSNil
  | VSCons Value VSubstitution
  deriving (Eq, Show)

data VPartialSubstitution
  = VPSNil
  | VPSCons (Maybe Value) VPartialSubstitution
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

mapMetaVar :: Value -> Value
mapMetaVar (VMetaVar lvl) = VNeutral $ NVar lvl
mapMetaVar val = val

vPSubstToList :: VPartialSubstitution -> [Maybe Value]
vPSubstToList VPSNil = []
vPSubstToList (VPSCons val rest) = vPSubstToList rest ++ [val]
