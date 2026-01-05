module Otus.Normalize.Meta.Value (
  MetaEnv,
  MetaClosure (..),
  MetaVType (..),
  MetaValueSeq,
  MetaNeutral (..),
  MetaValue (..),
  metaNeutralApp,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize.Env
import Otus.Normalize.Object.Value

newtype MetaEnv = MetaEnv MetaValueSeq
  deriving (Eq, Show)

instance SeqSize MetaEnv where
  size (MetaEnv s) = size s

instance Sequence MetaEnv where
  type Item MetaEnv = MetaValue
  fromSeq = MetaEnv
  toSeq (MetaEnv s) = s

instance Environment MetaEnv where
  type Element MetaEnv = MetaValue
  eempty = empty
  find idx env = env @? idx
  envLevel = LevelId . size
  pushN l env = env >< l

data MetaClosure = MetaClosure MetaEnv MetaTerm
  deriving (Eq, Show)

data MetaNeutral
  = MNVar LevelId
  | MNApp MetaNeutral MetaValueSeq
  deriving (Eq, Show)

type MetaValueSeq = Seq MetaValue

data MetaVType
  = MVFn MetaVType MetaVType
  | MVDyn MetaVType
  | MVInner VTelescope
  deriving (Eq, Show)

data MetaValue
  = MVNeutral MetaNeutral
  | MVLam MetaClosure
  | MVConsistent Int VProblem Record
  | MVErr
  deriving (Eq, Show)

instance Value MetaValue where
  type Neutral MetaValue = MetaNeutral
  vVar lvl = MVNeutral $ MNVar lvl
  fromNeutral = MVNeutral

metaNeutralApp :: MetaNeutral -> MetaValue -> MetaNeutral
metaNeutralApp n arg = case n of
  MNApp h args -> MNApp h (arg <| args)
  _ -> MNApp n $ singleton arg
