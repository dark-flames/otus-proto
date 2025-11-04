module Otus.Normalize.Err (
  EvalError (..),
  EvalResult,
) where

import Otus.Ast
import Otus.Common

data EvalError
  = Anyhow String
  | UnboundIndex IndexId
  | AppOnNonLambda
  | NatElimOnNonNat
  | JOnNonId
  | DBindOnNonDynamic
  deriving (Eq, Show)

type EvalResult = Result EvalError
