module Otus.TypeCheck.Judgement (
  MetaType,
  Type,
  MetaTypeKind (..),
  WfValue (..),
  WfComputation (..),
  WfTerm (..),
) where

import Otus.Ast
import Otus.Normalize

type MetaType = MetaValue

type Type = Value

data MetaTypeKind
  = Value
  | Computation
  deriving (Eq, Show)

data WfValue = WfValue
  { vtm :: MetaTerm,
    vtyOf :: MetaValue
  }
  deriving (Eq, Show)

data WfComputation = WfComputation
  { ctm :: MetaTerm,
    effOf :: EffectSet,
    ctyOf :: MetaValue
  }
  deriving (Eq, Show)

data WfTerm = WfTerm
  { tm :: Term,
    tyOf :: Type
  }
  deriving (Eq, Show)
