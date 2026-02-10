module Otus.Normalize.Value (
  EnvItem (..),
  Environment (..),
  freshVar,
  freshVar',
  freshMetaVar,
  freshMetaVar',
  EnvValue (..),
  Closure (..),
  ObjClosure,
  MetaClosure,
  MetaTyClosure,
  VTelescope,
  VRecord,
  VSequence,
  VConstraint (..),
  VProblem,
  Spine (..),
  Stuck (..),
  Value (..),
  MetaSpine (..),
  MetaVType (..),
  MetaValue (..),
) where

import Otus.Ast
import Otus.Common

-- Environment
data EnvItem
  = MetaVal MetaValue
  | ObjVal Value
  deriving (Eq, Show)

newtype Environment = Env
  { unEnv :: Seq EnvItem
  }
  deriving (Eq, Show)

envLevel :: Environment -> LevelId
envLevel = LevelId . size

freshVar :: Environment -> (Value, Environment)
freshVar env = (val, env ||> val)
  where
    val = Neutral (NVar $ envLevel env) SNil

freshVar' :: Environment -> Environment
freshVar' = snd . freshVar

freshMetaVar :: Environment -> (MetaValue, Environment)
freshMetaVar env = (val, env ||> val)
  where
    val = MNeutral (envLevel env) MSNil

freshMetaVar' :: Environment -> Environment
freshMetaVar' = snd . freshMetaVar

class EnvValue v where
  intoItem :: v -> EnvItem

  (||>) :: Environment -> v -> Environment
  e ||> val = Env (unEnv e |> intoItem val)

  (<||>) :: Environment -> Seq v -> Environment
  e <||> s = Env (unEnv e >< fmap intoItem s)

-- Closure
data Closure tm = Closure
  { clsEnv :: Environment,
    clsTm :: Term
  }
  deriving (Eq, Show)

type ObjClosure = Closure Term

type MetaClosure = Closure MetaTerm

type MetaTyClosure = Closure MetaType

-- Object
type VTelescope = Seq Value

type VRecord = Seq Value

type VSequence = Seq Value

type VProblem = Seq VConstraint

data VConstraint
  = VTmEq Int Value Value
  deriving (Eq, Show)

data Spine
  = SNil
  | SApp Spine Value
  | SFirst Spine
  | SRest Spine
  deriving (Eq, Show)

data Stuck
  = NVar LevelId
  | NSplicing LevelId MetaSpine
  deriving (Eq, Show)

data Value
  = Neutral Stuck Spine
  | VPi Value ObjClosure
  | VLam ObjClosure
  | VRecord VTelescope
  | VList VRecord
  | VType Int
  deriving (Eq, Show)

-- Meta
data MetaSpine
  = MSNil
  | MSApp MetaSpine MetaValue
  | MSForce MetaSpine
  | MSSeqAppL MetaSpine MetaValue
  | MSSeqAppR MetaValue MetaSpine
  deriving (Eq, Show)

data MetaVType
  = MVTNeutral LevelId (Seq MetaVType)
  | MVAbs MetaClosure
  | MVFn MetaValue MetaValue
  | MVDyn VTelescope VTelescope
  | MVStatic VTelescope
  | MVType
  | MVKind
  deriving (Eq, Show)

data MetaValue
  = MNeutral LevelId MetaSpine
  | MVLam MetaClosure
  | MVGuarded VProblem VSequence
  | MVQuote Record -- problem: term or value?
  deriving (Eq, Show)

instance Sized Environment where
  size = size . unEnv

instance Indexable Environment where
  type Item Environment = EnvItem
  (@?) e i = unEnv e @? i

instance EnvValue Value where
  intoItem = ObjVal

instance EnvValue MetaValue where
  intoItem = MetaVal
