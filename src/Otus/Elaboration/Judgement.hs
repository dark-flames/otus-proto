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

instance Substitutable WfObjTerm where
  subst :: WfObjTerm -> Subst -> WfObjTerm
  subst (WfObjTerm tm ty) sb = WfObjTerm (subst tm sb) ty

instance Substitutable WfMetaTerm where
  subst :: WfMetaTerm -> Subst -> WfMetaTerm
  subst (WfMetaTerm tm eff ty) sb = WfMetaTerm (subst tm sb) eff ty

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
