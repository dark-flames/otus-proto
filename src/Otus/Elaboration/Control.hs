module Otus.Elaboration.Control (
  ElabError (..),
  ElabResult,
  ElabResultT,
  fromEvalResult,
  doEvalCls,
  doEval,
  doReadback,
  tryPushTy,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Elaboration.Context
import Otus.Elaboration.Expr
import Otus.Normalize

data ElabError
  = Anyhow String
  | UnknownIdentifier String
  | DuplicateBinder String
  | ObjEvalError ObjEvalError
  | MetaEvalError MetaEvalError
  | StageError Expr Stage
  deriving (Eq, Show)

type ElabResult = Result ElabError

type ElabResultT = ResultT ElabError

fromEvalResult :: ObjEvalResult a -> ElabResult a
fromEvalResult = \case
  Success r -> Success r
  Failure e -> Failure $ ObjEvalError e

doEvalCls :: ObjValue -> Closure -> ElabResult ObjValue
doEvalCls arg cls = fromEvalResult $ evalClosure arg cls

doEval :: ObjTerm -> Context -> ElabResult ObjValue
doEval tm ctx = fromEvalResult $ evaluateObj tm (asEnv ctx)

doReadback :: LevelId -> ObjValue -> ElabResult ObjTerm
doReadback lvl val = fromEvalResult $ readback lvl val

tryPushTy :: String -> ObjValue -> Stage -> Context -> ElabResult (LevelId, Context)
tryPushTy strId vTy stage ctx = case pushTy strId vTy stage ctx of
  Just res -> return res
  Nothing -> throwError $ DuplicateBinder strId
