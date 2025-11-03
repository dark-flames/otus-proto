module Otus.Normalize.Err (
  EvalError (..),
  EvalResult,
) where

import Otus.Ast
import Otus.Common

data EvalError
  = Anyhow String
  | UnboundIndex Stage IndexId
  | AppOnNonLambda Stage
  | NatElimOnNonNat Stage
  | JOnNonId Stage
  | DBindOnNonDynamic
  deriving (Eq, Show)

type EvalResult = Result EvalError
