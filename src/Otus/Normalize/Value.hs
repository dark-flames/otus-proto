module Otus.Normalize.Value (
  Closure (..),
  VTelescope (..),
  VSubstitution (..),
  Value (..),
  Neutral (..),
  neutralApp,
  freshVar,
  pushVSubst,
) where

import Data.List.NonEmpty
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

data Neutral
  = NVar LevelId
  | NApp Neutral (NonEmpty Value)
  | NNatElim Value Value Neutral
  | NJ Value Value Neutral
  | VDBind Value Neutral
  deriving (Eq, Show)

data Value
  = VNeutral Neutral
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
  deriving (Eq, Show)

neutralApp :: Neutral -> Value -> Neutral
neutralApp n arg = case n of
  NApp h args -> NApp h (arg <| args)
  _ -> NApp n $ singleton arg

freshVar :: (CtxLike e Value) => e -> e
freshVar env = push env $ VNeutral $ NVar $ LevelId $ ctxLength env

pushVSubst :: (CtxLike e Value) => e -> VSubstitution -> e
pushVSubst env VSNil = env
pushVSubst env (VSCons val subst) = pushVSubst (push env val) subst
