module Otus.Ast.Term (
  Telescope (..),
  Substitution (..),
  Constraint (..),
  MetaDefinition (..),
  Signature (..),
  ObjTerm (..),
  ObjTermSeq,
  MetaType (..),
  MetaTerm (..),
) where

import Otus.Ast.Id
import Otus.Common

-- Telescope
newtype Telescope = Tele ObjTermSeq
  deriving (Eq, Show)

instance SeqSize Telescope where
  size (Tele s) = size s

instance Sequence Telescope where
  type Item Telescope = ObjTerm
  fromSeq = Tele
  toSeq (Tele s) = s

-- Substitution
newtype Substitution = Subst ObjTermSeq
  deriving (Eq, Show)

instance SeqSize Substitution where
  size (Subst s) = size s

instance Sequence Substitution where
  type Item Substitution = ObjTerm
  fromSeq = Subst
  toSeq (Subst s) = s

-- Signature
data Constraint
  = TyEq Telescope ObjTerm ObjTerm
  | TmEq Telescope ObjTerm ObjTerm ObjTerm
  deriving (Eq, Show)

data MetaDefinition
  = Unsolved
  | Guarded ObjTerm (Seq Constraint)
  | MSolved ObjTerm
  deriving (Eq, Show)

newtype Signature = Sig (Seq MetaDefinition)
  deriving (Eq, Show)

instance SeqSize Signature where
  size (Sig s) = size s

instance Sequence Signature where
  type Item Signature = MetaDefinition
  fromSeq = Sig
  toSeq (Sig s) = s

-- Obj
type ObjTermSeq = Seq ObjTerm

data ObjTerm
  = OVar IndexId
  | -- Pi type
    OPi ObjTerm ObjTerm
  | OLam ObjTerm
  | OApp ObjTerm ObjTerm
  | -- Universe
    OType Stage Universe
  deriving (Eq, Show)

-- Meta
data MetaType
  = MFn MetaType MetaType
  | MInner Telescope ObjTerm
  deriving (Eq, Show)

data MetaTerm
  = MVar IndexId
  | MLam MetaTerm
  | MApp MetaTerm MetaTerm
  | MGuarded Telescope Signature Substitution ObjTerm
  | MErr
  deriving (Eq, Show)
