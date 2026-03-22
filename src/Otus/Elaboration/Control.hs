module Otus.Elaboration.Control (
  ElaborationError (..),
  ElaborationResult,
  doEval,
) where

import Otus.Ast
import Otus.Common
import Otus.Elaboration.Context
import Otus.Normalize

data ElaborationError
  = EvaluationError EvalError String
  | HoasEvaluationError EvalError
  | UnboundName Name
  deriving (Show)

type ElaborationResult = Result ElaborationError

doEval :: (Evaluatable tm) => Context -> tm -> ElaborationResult (EvalRes tm)
doEval ctx tm = case evaluate tm (ctxEnv ctx) of
  Success v -> return v
  Failure e -> Failure $ EvaluationError e (show tm)
