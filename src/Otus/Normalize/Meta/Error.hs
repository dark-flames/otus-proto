module Otus.Normalize.Meta.Error (
  MetaEvalError (..),
  MetaEvalResult,
  MetaEvalResultT,
  MetaEvalMonad,
  fromObjResult,
  evalObjEvalMonad,
) where

import Control.Monad.State.Lazy (evalStateT)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Env
import Otus.Normalize.Meta.Value
import Otus.Normalize.Object.Error

data MetaEvalError
  = MetaAnyhow String
  | MetaUnboundIndex IndexId
  | MetaAppOnNonFn
  | MetaCombainOnNonInner
  | MetaCSubstOnNonInner
  | MetaObjError ObjEvalError
  deriving (Eq, Show)

type MetaEvalResult = Result MetaEvalError

type MetaEvalResultT = ResultT MetaEvalError

type MetaEvalMonad = EvalMonad MetaEvalError MetaValue

fromObjResult :: ObjEvalResult a -> MetaEvalResult a
fromObjResult = \case
  Success r -> Success r
  Failure err -> Failure $ MetaObjError err

evalObjEvalMonad :: ObjEvalMonad a -> MetaEvalResult a
evalObjEvalMonad m = fromObjResult $ evalStateT m eempty
