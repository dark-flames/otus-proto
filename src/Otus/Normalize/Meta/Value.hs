module Otus.Normalize.Meta.Value (
  MetaEnv,
  MetaValueSeq,
  MetaValue,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize.Env
import Otus.Normalize.Object.Value

type MetaEnv = Environment MetaValue

data MetaNeutral
  = MNVar LevelId
  | MNApp MetaNeutral
  deriving (Eq, Show)

type MetaValueSeq = Seq MetaValue

data MetaValue
  = VMNeutral MetaNeutral
  | VMFn MetaValue MetaValue
  | VMLam Closure
  deriving (Eq, Show)

instance Value MetaValue where
  vVar lvl = VMNeutral $ MNVar lvl
