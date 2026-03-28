module Otus.Normalize.Value (
  EnvItem (..),
  Domain (..),
  EnvVal (..),
  Environment (..),
  liftEnv,
  liftEnvN,
  envLevel,
  splitEnv,
  popEnv,
  HOAS (..),
  MetaHOAS,
  ObjHOAS,
  TeleHOAS,
  RecordHOAS,
  CstrHOAS,
  VTelescope (..),
  VTeleSequence (..),
  VRecord,
  VConstraint (..),
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
  | AmbiguousLevel LevelId

newtype Environment = Env
  { unEnv :: Seq EnvItem
  }

envLevel :: Environment -> LevelId
envLevel = LevelId . size

liftEnv :: Environment -> Environment
liftEnv = liftEnvN 1

liftEnvN :: Int -> Environment -> Environment
liftEnvN n env = env ||><| fmap f (fromList [s .. s + n])
  where
    s = size env
    f = AmbiguousLevel . LevelId

splitEnv :: Int -> Environment -> (Environment, Seq EnvItem)
splitEnv n (Env env) = (Env (Seq.take n env), Seq.drop n env)

popEnv :: Int -> Environment -> (Environment, Seq EnvItem)
popEnv n (Env env) = (Env (Seq.take (size env - n) env), Seq.drop (size env - n) env)

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

type CstrHOAS = HOAS VConstraint

data VTelescope
  = VTNil
  | VTCons Value TeleHOAS

newtype VTeleSequence = VTeleSeq
  { unVTele :: Seq Value
  }

type VRecord = Seq Value

data VConstraint
  = VCstrEmpty
  | VCstrTmEq VConstraint VTeleSequence Value Value Value
  | VCstrDef VConstraint Value

data Spine
  = SPNil
  | SPApp Spine Value
  | SPFirst Spine
  | SPRest Spine
  | SPJ Value Value Spine

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
  = MSPNil
  | MSPApp MetaSpine MetaValue
  | MSForce MetaSpine
  | MSBind MetaSpine MetaHOAS MetaHOAS
  | MSExt MetaSpine Int CstrHOAS RecordHOAS
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
  | MVGuard VConstraint RecordHOAS

instance Sized Environment where
  size = size . unEnv

instance Indexable Environment where
  type Item Environment = EnvItem
  (@?) e i = unEnv e @? i

instance Sized VTeleSequence where
  size = size . unVTele

instance Sized VConstraint where
  size VCstrEmpty = 0
  size (VCstrDef prev _) = size prev + 1
  size (VCstrTmEq prev _ _ _ _) = size prev + 1

instance Semigroup VConstraint where
  (<>) prev = \case
    VCstrEmpty -> prev
    VCstrDef rPrev ty -> VCstrDef (prev <> rPrev) ty
    VCstrTmEq rPrev tele lhs rhs ty -> VCstrTmEq (prev <> rPrev) tele lhs rhs ty

instance Domain Value where
  type Syntax Value = Term

  domVar lvl = Neutral (NVar lvl) SPNil

instance EnvVal Value where
  intoItem = ObjVal

instance Domain MetaValue where
  type Syntax MetaValue = MetaTerm

  domVar lvl = MNeutral lvl MSPNil

instance EnvVal MetaValue where
  intoItem = MetaVal

instance EnvVal EnvItem where
  intoItem = id

vvar :: LevelId -> Value
vvar lvl = Neutral (NVar lvl) SPNil

mvvar :: LevelId -> MetaValue
mvvar lvl = MNeutral lvl MSPNil

emptyEnv :: Environment
emptyEnv = Env mempty

trivalEnv :: LevelId -> Environment
trivalEnv (LevelId s) = Env (fromList $ map (AmbiguousLevel . LevelId) [0 .. s])

varSeq :: (Domain v) => [Int] -> Seq v
varSeq = fromList . map (domVar . LevelId)
