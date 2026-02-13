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
  = -- Value
    MVar IndexId
  | MU EffectSet MetaTerm
  | MThunk MetaTerm
  | MVType Int
  | ---- Embedding
    MLift Telescope
  | MQuote Record
  | MDyn Telescope Telescope
  | MNil Int
  | MExt MetaTerm Int Problem Sequence
  | -- Computation
    MPi MetaTerm EffectSet MetaTerm
  | MLam MetaTerm
  | MApp MetaTerm MetaTerm
  | MF MetaTerm
  | MReturn MetaTerm
  | MTrigger Effect MetaTerm
  | MLetIn MetaTerm MetaTerm MetaTerm
  | MForce MetaTerm
  | MCType Int
  | MSolve MetaTerm
  deriving (Eq)

instance Pretty MetaTerm where
  pretty = \case
    MVar i -> "%" ++ show (unIndex i)
    MU eff t -> "U(" ++ pretty eff ++ " ! " ++ pretty t ++ ")"
    MThunk t -> "[" ++ pretty t ++ "]"
    MVType i -> "VTy(" ++ show i ++ ")"
    MPi dom eff cod -> "Π(" ++ pretty dom ++ ")." ++ pretty eff ++ "!" ++ pretty cod ++ ")"
    MLam body -> "λ. (" ++ pretty body ++ ")"
    MApp f p -> pretty f ++ " ∘ " ++ pretty p
    MF t -> "F(" ++ pretty t ++ ")"
    MReturn t -> "return(" ++ pretty t ++ ")"
    MTrigger eff t -> "trigger(" ++ pretty eff ++ " ! " ++ pretty t ++ ")"
    MLetIn p b t -> "let (" ++ pretty p ++ ") in " ++ pretty b ++ " :: " ++ pretty t
    MForce t -> "force(" ++ pretty t ++ ")"
    MCType i -> "CTy(" ++ show i ++ ")"
    _ -> "undefinded"

instance Show MetaTerm where
  show = pretty
