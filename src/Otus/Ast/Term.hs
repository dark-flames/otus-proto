module Otus.Ast.Term (
  Telescope (..),
  Substitution (..),
  Constraint (..),
  MetaDefinition (..),
  Signature (..),
  Term (..),
  TermSeq,
) where

import Otus.Ast.Id
import Otus.Ast.Univ
import Otus.Common

-- Telescope
newtype Telescope = Tele TermSeq
  deriving (Eq, Show)

instance Sequence Telescope where
  type Item Telescope = Term
  fromSeq = Tele
  toSeq (Tele s) = s

-- Substitution
newtype Substitution = Subst TermSeq
  deriving (Eq, Show)

instance Sequence Substitution where
  type Item Substitution = Term
  fromSeq = Subst
  toSeq (Subst s) = s

-- Signature
data Constraint
  = TyEq Telescope Term Term
  | TmEq Telescope Term Term Term
  deriving (Eq, Show)

data MetaDefinition
  = MUnsolved
  | MGuarded Term (Seq Constraint)
  | MSolved Term
  deriving (Eq, Show)

newtype Signature = Sig (Seq MetaDefinition)
  deriving (Eq, Show)

instance Sequence Signature where
  type Item Signature = MetaDefinition
  fromSeq = Sig
  toSeq (Sig s) = s

type TermSeq = Seq Term

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
