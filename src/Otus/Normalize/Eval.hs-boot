module Otus.Normalize.Eval (
  evaluate,
) where

import Otus.Ast
import Otus.Normalize.Control
import Otus.Normalize.Env
import Otus.Normalize.Value

evaluate :: Term -> Environment -> EvalResult Value
