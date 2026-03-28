module Otus.Elaboration.Judgement (
  Type,
  MetaType,
  WfMetaTerm (..),
  WfObjTerm (..),
  Judgement (..),
  HybridType (..),
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

data WfObjTerm = WfObjTerm
  { jTm :: Term,
    jTy :: Type
  }

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

instance Judgement WfObjTerm where
  type Tm WfObjTerm = Term
  type TmTy WfObjTerm = Type

  tmOf (WfObjTerm tm _) = tm
  tyOf (WfObjTerm _ ty) = ty

data HybridType
  = MetaTy MetaType
  | ObjTy Type
