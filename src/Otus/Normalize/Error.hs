module Otus.Normalize.Error (
  EvalError (..),
) where

import Otus.Ast

data EvalError
  = Anyhow String
  | UnboundIndex IndexId
  | InvalidMetaVar IndexId
  | InvalidObjVar IndexId
  | AppOnNonLambda
  | ProjOnEmptyRecord
  | ProjOnNonRecord
  | SplicingNonMeta
  | BindOnNonComputation
  | ForceOnNonValue
  | SolveOnNonDyn
  deriving (Eq, Show)
