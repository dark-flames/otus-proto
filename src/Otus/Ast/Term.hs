module Otus.Ast.Term (
  Telescope (..),
  Substitution (..),
  Constraint (..),
  MetaDefinition (..),
  Signature (..),
  PartialRenaming (..),
  Term (..),
) where

import qualified Data.IntMap as IM

import Otus.Ast.Id
import Otus.Ast.Univ

newtype Telescope = Tele [Term]
  deriving (Eq, Show)

newtype Substitution = Subst [Term]
  deriving (Eq, Show)

data Constraint
  = TyEq Telescope Term Term
  | TmEq Telescope Term Term
  deriving (Eq, Show)

data MetaDefinition
  = MUnsolved
  | MGuarded Term [Constraint]
  | MSolved Term
  deriving (Eq, Show)

newtype Signature = Sig [MetaDefinition]
  deriving (Eq, Show)

data PartialRenaming
  = PRen
  { domSize :: LevelId,
    codSize :: LevelId,
    renamingMap :: IM.IntMap LevelId
  }
  deriving (Eq, Show)

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
    Local Telescope Term
  | Guarded Signature Term
  | Weaken PartialRenaming Term Term
  | Error
  | ---- Γ |- p : Local Δ T   Γ Δ , T |- n : Local Δ' B
    -----------------------------------------
    ---- Γ |- let open p in n : Local Δ,T,Δ' B
    LetOpen Term Term
  deriving (Eq, Show)
