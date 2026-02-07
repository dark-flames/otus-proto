module Otus.Ast.Term (
  Telescope (..),
  Record (..),
  Constraint (..),
  Problem (..),
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
newtype Record = Record ObjTermSeq
  deriving (Eq, Show)

instance SeqSize Record where
  size (Record s) = size s

instance Sequence Record where
  type Item Record = ObjTerm
  fromSeq = Record
  toSeq (Record s) = s

-- Signature
data Constraint
  = TmEq Int ObjTerm ObjTerm
  deriving (Eq, Show)

newtype Problem = Sig (Seq Constraint)
  deriving (Eq, Show)

instance SeqSize Problem where
  size (Sig s) = size s

instance Sequence Problem where
  type Item Problem = Constraint
  fromSeq = Sig
  toSeq (Sig s) = s

-- Obj
type ObjTermSeq = Seq ObjTerm

data ObjTerm
  = OVar IndexId
  | OMeta MetaId
  | -- Pi type
    OPi ObjTerm ObjTerm
  | OLam ObjTerm
  | OApp ObjTerm ObjTerm
  | -- Universe
    OType
  deriving (Eq, Show)

-- Meta
data MetaType
  = MFn MetaType MetaType
  | MInner Telescope
  deriving (Eq, Show)

data MetaTerm
  = MVar IndexId
  | MLam MetaTerm
  | MApp MetaTerm MetaTerm
  | MGuarded Int Problem Record
  | MProduct MetaTerm MetaTerm
  | MCSubst MetaTerm Problem Record ObjTerm
  | MErr
  deriving (Eq, Show)
