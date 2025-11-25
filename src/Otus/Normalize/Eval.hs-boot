module Otus.Normalize.Eval (
  evalClosure,
  evalClosure',
  evalClosureFresh,
  evalClosureFreshN,
  evalApp,
) where

import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Value

evalClosure :: Value -> Closure -> EvalResult Value
evalClosure' :: (Item l ~ Value, Sequence l) => l -> Closure -> EvalResult Value
evalClosureFresh :: Closure -> EvalResult (Value, Value)
evalClosureFreshN :: Int -> Closure -> EvalResult (Value, ValueSeq)
evalApp :: Value -> Value -> EvalResult Value
