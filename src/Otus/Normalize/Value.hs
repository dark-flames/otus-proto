module Otus.Normalize.Value (
  EnvItem (..),
  Domain (..),
  EnvVal (..),
  Environment (..),
  liftObjEnv,
  liftObjEnvN,
  liftMetaEnv,
  liftMetaEnvN,
  envLevel,
  splitEnv,
  HOAS (..),
  MetaHOAS,
  ObjHOAS,
  TeleHOAS,
  RecordHOAS,
  ProblemHOAS,
  VTelescope (..),
  VTeleSequence (..),
  VRecord,
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
  trivalEnv,
  varSeq,
) where

import Data.Sequence as Seq

import Otus.Ast
import Otus.Common
import Otus.Normalize.Error

-- Environment
data EnvItem
  = MetaVal MetaValue
  | ObjVal Value

newtype Environment = Env
  { unEnv :: Seq EnvItem
  }

envLevel :: Environment -> LevelId
envLevel = LevelId . size

liftObjEnv :: Environment -> Environment
liftObjEnv = liftObjEnvN 1

liftObjEnvN :: Int -> Environment -> Environment
liftObjEnvN n env = env ||><| fmap f (fromList [s .. s + n])
  where
    s = size env
    f = vvar . LevelId

liftMetaEnv :: Environment -> Environment
liftMetaEnv = liftMetaEnvN 1

liftMetaEnvN :: Int -> Environment -> Environment
liftMetaEnvN n env = env ||><| fmap f (fromList [s .. s + n])
  where
    s = size env
    f = mvvar . LevelId

splitEnv :: Int -> Environment -> (Environment, Seq EnvItem)
splitEnv n (Env env) = (Env (Seq.take (size env - n) env), Seq.drop (size env - n) env)

class EnvVal v where
  intoItem :: v -> EnvItem

  (||>) :: Environment -> v -> Environment
  e ||> val = Env (unEnv e |> intoItem val)

  (||><|) :: Environment -> Seq v -> Environment
  e ||><| s = Env (unEnv e >< fmap intoItem s)

  pushEnv :: v -> Environment -> Environment
  pushEnv val e = e ||> val

  pushEnvN :: Seq v -> Environment -> Environment
  pushEnvN s e = e ||><| s

class (EnvVal v) => Domain v where
  type Syntax v

  domVar :: LevelId -> v

-- Closure
newtype HOAS val = HOAS
  { evalHOAS :: (Environment -> Environment) -> Result EvalError val
  }

type MetaHOAS = HOAS MetaValue

-- Object
type RecordHOAS = HOAS VRecord

type ObjHOAS = HOAS Value

type TeleHOAS = HOAS VTelescope

type ProblemHOAS = HOAS VProblem

data VTelescope
  = VTNil
  | VTCons Value TeleHOAS

newtype VTeleSequence = VTeleSeq
  { unVTele :: Seq Value
  }

type VRecord = Seq Value

type VProblem = Seq VConstraint

data VConstraint
  = VTmEq VTeleSequence Value Value Value

data Spine
  = SNil
  | SApp Spine Value
  | SFirst Spine
  | SRest Spine
  | SJ Value Value Spine

data Stuck
  = NVar LevelId
  | NSplicing LevelId

data Value
  = Neutral Stuck Spine
  | VPi Value ObjHOAS
  | VLam ObjHOAS
  | VRecord VTelescope
  | VList VRecord
  | VId Value Value Value
  | VRefl
  | VType Int

-- Meta
data MetaSpine
  = MSNil
  | MSApp MetaSpine MetaValue
  | MSForce MetaSpine
  | MSBind MetaSpine MetaHOAS MetaHOAS
  | MSAbsMeta Value MetaSpine
  | MSExt MetaSpine Int ProblemHOAS RecordHOAS
  | MSSolve MetaSpine

data MetaValue
  = MNeutral LevelId MetaSpine
  | -- Comptation
    MVPi MetaValue EffectSet MetaHOAS
  | MVLam MetaHOAS
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
  | MVDyn VTelescope
  | MVGuard VTeleSequence VProblem RecordHOAS

instance Sized Environment where
  size = size . unEnv

instance Indexable Environment where
  type Item Environment = EnvItem
  (@?) e i = unEnv e @? i

instance Sized VTeleSequence where
  size = size . unVTele

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

trivalEnv :: LevelId -> Environment
trivalEnv (LevelId s) =
  let
    vs :: Seq Value
    vs = varSeq [0 .. s]
  in
    emptyEnv ||><| vs

varSeq :: (Domain v) => [Int] -> Seq v
varSeq = fromList . map (domVar . LevelId)
