module Otus.Normalize.Eval (
  constraintAsRefl,
  evaluateNeutral,
  evaluateFirst,
  evaluateRest,
  evaluateApp,
  evaluateMApp,
) where

import Otus.Normalize.Control
import Otus.Normalize.Value

constraintAsRefl :: VConstraint -> Value
evaluateNeutral :: Value -> Spine -> EvalResult Value
evaluateFirst :: Value -> EvalResult Value
evaluateRest :: Value -> EvalResult Value
evaluateApp :: Value -> Value -> EvalResult Value
evaluateMApp :: MetaValue -> MetaValue -> EvalResult MetaValue
