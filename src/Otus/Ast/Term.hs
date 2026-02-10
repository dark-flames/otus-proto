module Otus.Ast.Term (
  Telescope,
  Record,
  Sequence,
  Constraint (..),
  Problem,
  Term (..),
  Effect (..),
  EffectSet,
  MetaType (..),
  MetaTerm (..),
) where

import Data.Set (Set)

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

data Effect
  = Unification
  | NonTermination
  deriving (Eq, Show)

type EffectSet = Set Effect

data MetaType
  = MTyVar IndexId
  | MAbs MetaTerm
  | MTApp MetaTerm MetaTerm
  | MFn MetaTerm MetaTerm
  | MDyn Telescope Telescope
  | MStatic Telescope
  | MType
  | MKind
  deriving (Eq, Show)

data MetaTerm
  = MVar IndexId
  | MLam MetaTerm
  | MApp MetaTerm MetaTerm
  | MGuarded Problem Sequence
  | MQuote Record
  | MForce Term
  | MSeqApp Term Term
  deriving (Eq, Show)
