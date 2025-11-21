module Otus.Ast.Term (
  Telescope (..),
  Substitution (..),
  Constraint (..),
  GuardedSubstSeg (..),
  GuardedSubstitution (..),
  PartialRenaming (..),
  Term (..),
) where

import Otus.Ast.Id
import Otus.Ast.Univ

import qualified Data.IntMap as IM

newtype Telescope = Tele [Term]
  deriving (Eq, Show)

newtype Substitution = Subst [Term]
  deriving (Eq, Show)

data Constraint
  = TyEq Telescope Term Term
  | TmEq Telescope Term Term
  deriving (Eq, Show)

data GuardedSubstSeg
  = Unsolved
  | Solved Term [Constraint]
  deriving (Eq, Show)

newtype GuardedSubstitution = GSubst [GuardedSubstSeg]
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
  | -- Identity type
    Id Term Term Term
  | Refl
  | ---- J : {A : Set} {x : A}
    ---- → (P : (y : A) → x ≡ y → Set)
    ---- → P x refl
    ---- → {y : A} (p : x ≡ y)
    ---- → P y p
    J Term Term Term
  | -- Universe
    Type Stage Universe
  | -- Object
    Dynamic Telescope Term
  | Ok Substitution Term
  | TyErr
  | DBind Term Term
  | Force Term
  | -- Meta
    Lift Term
  | Quote Term
  | Local Telescope Term
  | Guarded GuardedSubstitution Term
  | Weaken PartialRenaming Term Term
  | Error
  | ---- Γ |- p : Local Δ T   Γ Δ , T |- n : Local Δ' B
    -----------------------------------------
    ---- Γ |- let open p in n : Local Δ,T,Δ' B
    LetOpen Term Term
  deriving (Eq, Show)
