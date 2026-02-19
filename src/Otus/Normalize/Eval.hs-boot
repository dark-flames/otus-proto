module Otus.Normalize.Eval (
  evaluateNeutral,
  evaluateApp,
  evaluateMApp,
) where

import Otus.Normalize.Control
import Otus.Normalize.Value

evaluateNeutral :: Value -> Spine -> EvalResult Value
evaluateApp :: Value -> Value -> EvalResult Value
evaluateMApp :: MetaValue -> MetaValue -> EvalResult MetaValue
