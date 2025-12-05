module Otus.Normalize.Meta.Error (
  MetaEvalError (..),
  MetaEvalResult,
  MetaEvalResultT,
  MetaEvalMonad,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Meta.Value

data MetaEvalError
  = MetaAnyhow String
  | MetaUnboundIndex IndexId
  deriving (Eq, Show)

type MetaEvalResult = Result MetaEvalError

type MetaEvalResultT = ResultT MetaEvalError

type MetaEvalMonad = EvalMonad MetaEvalError MetaValue
