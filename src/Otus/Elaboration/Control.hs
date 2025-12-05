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
  | EvalError EvalError
  | StageError Expr Stage
  deriving (Eq, Show)

type ElabResult = Result ElabError

type ElabResultT = ResultT ElabError

fromEvalResult :: EvalResult a -> ElabResult a
fromEvalResult = \case
  Success r -> Success r
  Failure e -> Failure $ EvalError e

doEvalCls :: Value -> Closure -> ElabResult Value
doEvalCls arg cls = fromEvalResult $ evalClosure arg cls

doEval :: Term -> Context -> ElabResult Value
doEval tm ctx = fromEvalResult $ evaluate tm (asEnv ctx)

doReadback :: LevelId -> Value -> ElabResult Term
doReadback lvl val = fromEvalResult $ readback lvl val

tryPushTy :: String -> Value -> Stage -> Context -> ElabResult (LevelId, Context)
tryPushTy strId vTy stage ctx = case pushTy strId vTy stage ctx of
  Just res -> return res
  Nothing -> throwError $ DuplicateBinder strId
