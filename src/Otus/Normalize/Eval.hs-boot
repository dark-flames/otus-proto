module Otus.Normalize.Eval (
  Evaluatable (..),
  absRefl,
  evaluateNeutral,
  evaluateFirst,
  evaluateRest,
  evaluateApp,
  evaluateMApp,
  evaluateTerm,
) where

import Otus.Ast
import Otus.Normalize.Control
import Otus.Normalize.Value

class (Show t) => Evaluatable t where
  type EvalRes t

  evaluate :: t -> Environment -> EvalResult (EvalRes t)

  makeCls :: t -> Environment -> HOAS (EvalRes t)
  makeCls t env = HOAS (\f -> evaluate t (f env))

absRefl :: Int -> Value
evaluateNeutral :: Value -> Spine -> EvalResult Value
evaluateFirst :: Value -> EvalResult Value
evaluateRest :: Value -> EvalResult Value
evaluateApp :: Value -> Value -> EvalResult Value
evaluateMApp :: MetaValue -> MetaValue -> EvalResult MetaValue
evaluateTerm :: Term -> Environment -> EvalResult Value
