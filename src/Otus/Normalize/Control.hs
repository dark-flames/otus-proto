module Otus.Normalize.Control (
  EvalMonad,
  EvalResult,
  EvalResultT,
) where

import Control.Monad.State.Lazy (StateT)

import Otus.Common
import Otus.Normalize.Error
import Otus.Normalize.Value

type EvalResult = Result EvalError

type EvalResultT = ResultT EvalError

type EvalMonad = StateT Environment EvalResult
