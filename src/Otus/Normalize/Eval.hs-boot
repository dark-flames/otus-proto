module Otus.Normalize.Eval (
  doEvaluate,
  evalClosure,
  evalClosure',
) where

import Otus.Ast
import Otus.Normalize.Control
import Otus.Normalize.Value

doEvaluate :: Term -> EvalMonad Value
evalClosure :: Value -> Closure -> EvalResult Value
evalClosure' :: [Value] -> Closure -> EvalResult Value
