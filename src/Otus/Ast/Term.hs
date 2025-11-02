module Otus.Ast.Term
  ( Term (..),
  )
where

import Otus.Ast.Id
import Otus.Ast.Univ

data Telescope
  = TNil
  | TCons Term Telescope
  deriving (Show, Eq)

data PartialSubstitution
  = PSNil
  | PSCons (Maybe Term) PartialSubstitution
  deriving (Show, Eq)

data Substitution
  = SNil
  | SCons Term Substitution
  deriving (Show, Eq)

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
  | NatElim Term Term Term Term
  | -- Identity type
    Id Term Term Term
  | Refl
  | J Term Term Term
  | -- Meta
    Lift Term
  | Quote Term
  | Local Telescope Term
  | Partial PartialSubstitution Term
  | Error
  | Bind Term
  | Unify Term Term
  | -- Object
    Dynamic Telescope Term
  | Ok Substitution Term
  | TyErr
  | DBind Term
  | Force Term
  | -- Universe
    Type Stage Universe
  deriving (Show, Eq)
