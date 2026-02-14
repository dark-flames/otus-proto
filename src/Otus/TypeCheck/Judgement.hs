module Otus.TypeCheck.Judgement (
  MetaType,
  Type,
  WfMetaTerm (..),
  WfTerm (..),
  Judgement (..),
  wfMetaValue,
  effOf,
) where

import Otus.Ast
import Otus.Normalize

type MetaType = MetaValue

type Type = Value

data WfMetaTerm = WfMetaTerm
  { jTm :: MetaTerm,
    jEff :: EffectSet,
    jTy :: MetaType
  }

wfMetaValue :: MetaTerm -> MetaType -> WfMetaTerm
wfMetaValue t = WfMetaTerm t mempty

data WfTerm = WfTerm
  { jTm :: Term,
    jTy :: Type
  }
  deriving (Eq, Show)

class Judgement j where
  type Tm j
  type TmTy j

  tmOf :: j -> Tm j
  tyOf :: j -> TmTy j

instance Judgement WfMetaTerm where
  type Tm WfMetaTerm = MetaTerm
  type TmTy WfMetaTerm = MetaType

  tmOf (WfMetaTerm t _ _) = t
  tyOf (WfMetaTerm _ _ t) = t

instance Judgement WfTerm where
  type Tm WfTerm = Term
  type TmTy WfTerm = Type

  tmOf (WfTerm tm _) = tm
  tyOf (WfTerm _ ty) = ty

effOf :: WfMetaTerm -> EffectSet
effOf (WfMetaTerm _ e _) = e
