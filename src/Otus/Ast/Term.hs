module Otus.Ast.Term (
  Telescope,
  Record,
  Sequence,
  Constraint (..),
  Problem,
  Term (..),
  EffectSet,
  MetaTerm (..),
) where

import Otus.Ast.Effect
import Otus.Ast.Id
import Otus.Common

type Telescope = Seq Term

type Record = Seq Term

type Sequence = Seq Term

type Problem = Seq Constraint

data Constraint
  = TmEq Int Term Term
  deriving (Eq, Show)

data Term
  = Var IndexId
  | -- Pi type
    Pi Term Term
  | Lam Term
  | App Term Term
  | -- Record
    Record Telescope
  | List Record
  | First Term
  | Rest Term
  | -- Embedding
    Splicing MetaTerm
  | -- Universe
    Type Int
  deriving (Eq, Show)

data MetaTerm
  = MVar IndexId
  | MPi MetaTerm EffectSet MetaTerm
  | MLam MetaTerm
  | MApp MetaTerm MetaTerm
  | -- CBPV
    MF MetaTerm
  | MReturn MetaTerm
  | MTrigger Effect MetaTerm
  | MLetIn MetaTerm MetaTerm MetaTerm
  | MU EffectSet MetaTerm
  | MThunk MetaTerm
  | MForce MetaTerm
  | MCType Int
  | MVType Int
  | -- Embedding
    MLift Telescope
  | MQuote Record
  | MDyn Telescope Telescope
  | MNil Int
  | MExt MetaTerm Int Problem Sequence
  | MSolve MetaTerm
  deriving (Eq, Show)
