module Otus.Normalize.Value (
  EnvItem (..),
  Domain (..),
  EnvVal (..),
  Environment (..),
  objLiftEnv,
  envLevel,
  Closure (..),
  ObjClosure,
  MetaClosure,
  TeleClosure,
  VTelescope (..),
  VRecord,
  VSequence,
  VConstraint (..),
  VProblem,
  Spine (..),
  Stuck (..),
  Value (..),
  MetaSpine (..),
  MetaValue (..),
  vvar,
  mvvar,
  emptyEnv,
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

objLiftEnv :: Int -> Environment -> Environment
objLiftEnv n env = env ||><| fmap f (fromList [s .. s + n])
  where
    s = size env
    f = vvar . LevelId

class EnvVal v where
  intoItem :: v -> EnvItem

  (||>) :: Environment -> v -> Environment
  e ||> val = Env (unEnv e |> intoItem val)

  (||><|) :: Environment -> Seq v -> Environment
  e ||><| s = Env (unEnv e >< fmap intoItem s)

class (EnvVal v, Eq v, Show v) => Domain v where
  type Syntax v

  domVar :: LevelId -> v

-- Closure
data Closure tm = Closure
  { clsEnv :: Environment,
    clsTm :: tm
  }
  deriving (Eq, Show)

type ObjClosure = Closure Term

type MetaClosure = Closure MetaTerm

-- Object
type TeleClosure = Closure Telescope

data VTelescope
  = VTNil
  | VTCons Value TeleClosure
  deriving (Eq, Show)

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
  | NSplicing LevelId
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
  | MSBind MetaSpine MetaClosure MetaClosure
  | MSExt MetaSpine VProblem Environment Sequence
  | MSSolveWith MetaSpine VProblem (Seq (Environment, Sequence))
  deriving (Eq, Show)

data MetaValue
  = MNeutral LevelId MetaSpine
  | -- Comptation
    MVPi MetaValue EffectSet MetaClosure
  | MVLam MetaClosure
  | MVF MetaValue
  | MVReturn MetaValue
  | MVTrigger Effect
  | MVCType Int
  | -- Value
    MVU EffectSet MetaValue
  | MVThunk MetaValue
  | MVVType Int
  | MVLift VTelescope
  | MVQuote VRecord
  | MVDyn VTelescope TeleClosure
  | MVNil Int
  | MVExt MetaValue Int VProblem Environment Sequence
  deriving (Eq, Show)

instance Sized Environment where
  size = size . unEnv

instance Indexable Environment where
  type Item Environment = EnvItem
  (@?) e i = unEnv e @? i

instance Domain Value where
  type Syntax Value = Term

  domVar lvl = Neutral (NVar lvl) SNil

instance EnvVal Value where
  intoItem = ObjVal

instance Domain MetaValue where
  type Syntax MetaValue = MetaTerm

  domVar lvl = MNeutral lvl MSNil

instance EnvVal MetaValue where
  intoItem = MetaVal

instance EnvVal EnvItem where
  intoItem = id

vvar :: LevelId -> Value
vvar lvl = Neutral (NVar lvl) SNil

mvvar :: LevelId -> MetaValue
mvvar lvl = MNeutral lvl MSNil

emptyEnv :: Environment
emptyEnv = Env mempty
