module Otus.Normalize.Eval (
  evalClosure,
  evalClosure',
  evalClosureFresh,
  evalClosureFreshN,
  evalApp,
) where

import Otus.Normalize.Control
import Otus.Normalize.Value

evalClosure :: Value -> Closure -> EvalResult Value
evalClosure' :: ValueSeq -> Closure -> EvalResult Value
evalClosureFresh :: Closure -> EvalResult (Value, Value)
evalClosureFreshN :: Int -> Closure -> EvalResult (Value, ValueSeq)
evalApp :: Value -> Value -> EvalResult Value
