module Otus.Ast.Term (
  Telescope (..),
  Substitution (..),
  Term (..),
) where

import Otus.Ast.Id
import Otus.Ast.Univ

newtype Telescope = Tele [Term]
  deriving (Eq, Show)

newtype Substitution = Subst [Term]
  deriving (Eq, Show)

data Term
  = Var IndexId
  | -- Pi type
    Pi Term Term
  | Lam Term
  | App Term Term
  | -- Natural numbers
    Nat
  | Zero
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
  | Partial Telescope Substitution Term
  | Error
  | Bind Term Term
  | Unify Term Term
  deriving (Eq, Show)
