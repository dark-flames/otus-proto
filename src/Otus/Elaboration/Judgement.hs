module Otus.Elaboration.Judgement (
  WfMetaTerm (..),
  WfObjTerm (..),
  HybridTerm (..),
  Judgement (..),
  HybridType (..),
  wfObjTerm,
  wfMetaValue,
  wfMetaComputation,
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

data HybridTerm
  = ObjTerm WfObjTerm
  | MetaTerm WfMetaTerm

data HybridType
  = MetaTy MetaType
  | ObjTy Type

wfObjTerm :: Term -> Type -> HybridTerm
wfObjTerm tm ty = ObjTerm $ WfObjTerm tm ty

wfMetaValue :: MetaTerm -> MetaType -> HybridTerm
wfMetaValue tm ty = MetaTerm $ WfMetaTerm tm mempty ty

wfMetaComputation :: MetaTerm -> EffectSet -> MetaType -> HybridTerm
wfMetaComputation tm eff ty = MetaTerm $ WfMetaTerm tm eff ty
