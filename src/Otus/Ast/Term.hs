module Otus.Ast.Term (
  Telescope (..),
  Substitution (..),
  Constraint (..),
  MetaDefinition (..),
  Signature (..),
  PartialRenaming (..),
  Term (..),
  TermSeq,
) where

import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq

import Otus.Ast.Id
import Otus.Ast.Univ

newtype Telescope = Tele TermSeq
  deriving (Eq, Show)

newtype Substitution = Subst TermSeq
  deriving (Eq, Show)

data Constraint
  = TyEq Telescope Term Term
  | TmEq Telescope Term Term
  deriving (Eq, Show)

data MetaDefinition
  = MUnsolved
  | MGuarded Term (Seq.Seq Constraint)
  | MSolved Term
  deriving (Eq, Show)

newtype Signature = Sig (Seq.Seq MetaDefinition)
  deriving (Eq, Show)

data PartialRenaming
  = PRen
  { domSize :: LevelId,
    codSize :: LevelId,
    renamingMap :: IM.IntMap LevelId
  }
  deriving (Eq, Show)

type TermSeq = Seq.Seq Term

data Term
  = Var IndexId
  | -- Pi type
    Pi Term Term
  | Lam Term
  | App Term Term
  | -- Natural numbers
    Nat Stage
  | Zero Stage
  | Succ Term
  | ---- NatElim : {P : Nat → Set}
    ---- → P 0
    ---- → ((n : Nat) → (P n) → P (1 + n))
    ---- → (m : Nat) → P m
    NatElim Term Term Term
  | -- Universe
    Type Stage Universe
  | -- Object
    Dynamic Telescope Term
  | Ok Substitution Term
  | TyErr
  | DBind Term Term
  | Force Term
  | -- Meta
    Local Telescope Telescope Term
  | Guarded Int Signature Term
  | Error
  | Assign Int Signature Term
  | Open Term Term
  deriving (Eq, Show)
