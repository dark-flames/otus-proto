module Otus.Normalize.Object.Value (
  MetaEntry (..),
  MetaContext (..),
  solveMeta,
  buildMetaCtx,
  ObjEnv (..),
  findMeta,
  ObjClosure (..),
  VTelescope (..),
  VRecord (..),
  VConstraint (..),
  VProblem (..),
  ConstraintSubstitution (CSubst),
  ObjValue (.., OVVar, OVMeta),
  ObjValueSeq,
  ObjNeutral (..),
  objNeutralApp,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize.Env

data MetaEntry
  = Solved ObjValue
  | UnSolved
  deriving (Eq, Show)

newtype MetaContext = MetaCtx (Seq MetaEntry)
  deriving (Eq, Show)

instance SeqSize MetaContext where
  size (MetaCtx s) = size s

instance Sequence MetaContext where
  type Item MetaContext = MetaEntry
  fromSeq = MetaCtx
  toSeq (MetaCtx s) = s

instance SeqModify MetaContext where
  adjust f idx l = adjust f (intoLeftIndex l idx) l
  update idx v l = update (intoLeftIndex l idx) v l

solveMeta :: MetaId -> ObjValue -> MetaContext -> MetaContext
solveMeta m val = update (unMeta m) (Solved val)

buildMetaCtx :: Int -> MetaContext
buildMetaCtx n = cycleTaking n [UnSolved]

data ObjEnv = ObjEnv
  { metaEnv :: MetaContext,
    varEnv :: ObjValueSeq
  }
  deriving (Eq, Show)

instance SeqSize ObjEnv where
  size = size . varEnv

instance Environment ObjEnv where
  type Element ObjEnv = ObjValue
  eempty = ObjEnv empty empty
  find idx env = varEnv env @? idx
  envLevel = LevelId . size
  pushN vals (ObjEnv menv venv) = ObjEnv menv (venv >< vals)

findMeta :: MetaId -> ObjEnv -> Maybe MetaEntry
findMeta m env = metaEnv env @? unMeta m

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

-- problem
newtype VProblem = VProb (Seq VConstraint)
  deriving (Eq, Show)

instance SeqSize VProblem where
  size (VProb s) = size s

instance Sequence VProblem where
  type Item VProblem = VConstraint
  fromSeq = VProb
  toSeq (VProb s) = s

data ConstraintSubstitution
  = CSubst
  { dom :: Int,
    problem :: VProblem,
    subst :: Record
  }
  deriving (Eq, Show)

-- neutral
data ObjNeutral
  = ONFlex MetaId ObjValueSeq
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

pattern OVVar :: LevelId -> ObjValue
pattern OVVar lvl = OVNeutral (ONRigid lvl Empty)

pattern OVMeta :: MetaId -> ObjValue
pattern OVMeta m = OVNeutral (ONFlex m Empty)

instance Value ObjValue where
  type Neutral ObjValue = ObjNeutral
  vVar lvl = OVNeutral $ ONRigid lvl empty
  fromNeutral = OVNeutral

objNeutralApp :: ObjNeutral -> ObjValue -> ObjNeutral
objNeutralApp n arg = case n of
  ONFlex h args -> ONFlex h (args |> arg)
  ONRigid h args -> ONRigid h (args |> arg)
