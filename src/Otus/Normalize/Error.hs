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
  | JOnNonId
  | SplicingNonMeta
  | BindOnNonComputation
  | ForceOnNonValue
  | AbsOnNonDyn
  | ExtOnNonDyn
  | SolveOnNonDyn
  | UnsolvableTmEq
  deriving (Eq, Show)
