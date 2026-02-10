module Otus.Normalize.Error (
  EvalError (..),
) where

import Otus.Ast

data EvalError
  = Anyhow String
  | UnboundIndex IndexId
  | InvalidMetaVar IndexId
  | AppOnNonLambda
  | ProjOnEmptyRecord
  | ProjOnNonRecord
  | SplicingNonMeta
  deriving (Eq, Show)
