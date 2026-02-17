module Otus.TypeCheck.Error (
  TypeError (..),
  TypeCheckResult,
  doEval,
  doEval',
  doEvalHOAS,
  doEvalApp,
  doEvalMApp,
  doQuote,
  doQuote',
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize
import Otus.TypeCheck.Context

data TypeError
  = EvalError EvalError String
  | HOASEvalError EvalError
  | ReadbackError EvalError
  | AppEvalError EvalError String String
  | CannotInferIndex
  | CannotInferObjAsMeta
  | CannotInferMetaAsObj
  | CannotCheckRecord Record Telescope
  | CannotCheckAsMetaType MetaTerm MetaTerm
  | CannotCheckAsThunk MetaTerm MetaTerm
  | CannotCheckRefl Term Term Term
  | CannotForce MetaTerm MetaTerm
  | CannotBindOn MetaTerm MetaTerm
  | CannotSplicing MetaTerm MetaTerm
  | Unify Term Term Term
  | ComputationUnify MetaTerm MetaTerm MetaTerm
  | ValueUnify MetaTerm MetaTerm MetaTerm
  | CannotInferValue MetaTerm
  | CannotInferComputation MetaTerm
  | CannotInferTerm Term
  | ComputationEffErr MetaTerm EffectSet EffectSet
  | CannotCheckAsType String
  | ExpectedToBeMetaTy MetaTerm
  | ExpectedToBeFn Term Term
  | ExpectedToBeMetaFn MetaTerm MetaTerm
  | ExpectedToBeNonEmptyRecord Term Term
  | ExpectedToBeIdeneity Term Term
  | AnyhowTyErr String
  deriving (Show)

type TypeCheckResult = Result TypeError

doEval
  :: (Evaluatable tm)
  => Context -> tm -> TypeCheckResult (EvalRes tm)
doEval ctx tm =
  case evaluate tm (ctxEnv ctx) of
    Success v -> return v
    Failure e -> Failure $ EvalError e (show tm)

doEval'
  :: (EnvVal v, Evaluatable tm)
  => Context -> [v] -> tm -> TypeCheckResult (EvalRes tm)
doEval' ctx p tm =
  case evaluate tm (ctxEnv ctx ||><| fromList p) of
    Success v -> return v
    Failure e -> Failure $ EvalError e (show tm)

doEvalHOAS :: (Environment -> Environment) -> HOAS v -> TypeCheckResult v
doEvalHOAS f hoas = case evalHOAS hoas f of
  Success v -> return v
  Failure e -> Failure $ HOASEvalError e

doQuote :: (Quotable v) => Context -> v -> TypeCheckResult (QuoteRes v)
doQuote ctx = doQuote' (ctxLvl ctx)

doQuote' :: (Quotable v) => LevelId -> v -> TypeCheckResult (QuoteRes v)
doQuote' lvl v = case quote lvl v of
  Success t -> return t
  Failure e -> Failure $ ReadbackError e

doEvalApp :: LevelId -> Value -> Value -> TypeCheckResult Value
doEvalApp lvl f p = case evaluateApp f p of
  Success v -> return v
  Failure e -> do
    fTm <- doQuote' lvl f
    pTm <- doQuote' lvl p
    Failure $ AppEvalError e (show fTm) (show pTm)

doEvalMApp :: LevelId -> MetaValue -> MetaValue -> TypeCheckResult MetaValue
doEvalMApp lvl f p = case evaluateMApp f p of
  Success v -> return v
  Failure e -> do
    fTm <- doQuote' lvl f
    pTm <- doQuote' lvl p
    Failure $ AppEvalError e (show fTm) (show pTm)
