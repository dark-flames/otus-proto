module Otus.TypeCheck.Error (
  TypeError (..),
  TypeCheckResult,
  doEval,
  doEval',
  doEvalClosure,
  doEvalClosureFresh,
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
  | ReadbackError EvalError String
  | AppEvalError EvalError String String
  | CannotInferIndex
  | CannotInferObjAsMeta
  | CannotInferMetaAsObj
  | CannotCheckRecord Record Telescope
  | CannotCheckAsMetaType MetaTerm MetaTerm
  | CannotCheckAsThunk MetaTerm MetaTerm
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

doEvalClosure
  :: (EnvVal (ClsParam tm), Evaluatable tm)
  => ClsParam tm -> Closure tm -> TypeCheckResult (EvalRes tm)
doEvalClosure val cls = case evaluateClosure val cls of
  Success v -> return v
  Failure e -> Failure $ EvalError e (show (clsTm cls))

doEvalClosureFresh
  :: (Domain (ClsParam tm), Evaluatable tm)
  => Closure tm -> TypeCheckResult (EvalRes tm)
doEvalClosureFresh cls = case evaluateClosureFresh cls of
  Success v -> return v
  Failure e -> Failure $ EvalError e (show (clsTm cls))

doQuote :: (Quotable v) => Context -> v -> TypeCheckResult (QuoteRes v)
doQuote ctx = doQuote' (ctxLvl ctx)

doQuote' :: (Quotable v) => LevelId -> v -> TypeCheckResult (QuoteRes v)
doQuote' lvl v = case quote lvl v of
  Success t -> return t
  Failure e -> Failure $ ReadbackError e (show v)

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
