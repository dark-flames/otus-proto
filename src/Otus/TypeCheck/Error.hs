module Otus.TypeCheck.Error (
  TypeError (..),
  TypeCheckResult,
  doEval,
  doEval',
  doEvalVTeleSeq,
  doEvalHOAS,
  doEvalApp,
  doEvalMApp,
  doQuote,
  doQuote',
  doConv,
  doIntoTeleSequence,
) where

import Control.Monad.Error.Class (throwError)

import Otus.Ast
import Otus.Common
import Otus.Normalize
import Otus.TypeCheck.Context

data TypeError
  = EvaluationError EvalError String
  | HoasEvaluationError EvalError
  | ConversionError EvalError String String
  | QuoteError EvalError
  | ApplicationEvaluationError EvalError String String
  | CannotInferIndex
  | CannotInferObjectAsMeta
  | CannotInferMetaAsObject
  | CannotInferObjectTerm Term
  | CannotInferMetaTerm MetaTerm
  | CannotInferComputation MetaTerm
  | ExpectedMetaType MetaTerm
  | ExpectedFunctionType Term Term
  | ExpectedMetaFunctionType MetaTerm MetaTerm
  | ExpectedDynamicType MetaTerm MetaTerm
  | ExpectedNonEmptyRecordType Term Term
  | ExpectedIdentityType Term Term
  | CannotCheckRecord Record Telescope
  | CannotCheckAsMetaType MetaTerm MetaTerm
  | CannotCheckAsThunk MetaTerm MetaTerm
  | CannotCheckRefl Term Term Term
  | CannotForce MetaTerm MetaTerm
  | CannotBindOn MetaTerm MetaTerm
  | CannotSplice MetaTerm MetaTerm
  | UnificationFailure String String
  | ComputationUnify MetaTerm MetaTerm MetaTerm
  | ValueUnify MetaTerm MetaTerm MetaTerm
  | ComputationEffectError MetaTerm EffectSet EffectSet
  | CannotCheckAsType String
  | UnexpectedLift MetaTerm Int
  | TypeCheckInternalError String
  deriving (Show)

type TypeCheckResult = Result TypeError

doEval
  :: (Evaluatable tm)
  => Context -> tm -> TypeCheckResult (EvalRes tm)
doEval ctx tm =
  case evaluate tm (ctxEnv ctx) of
    Success v -> return v
    Failure e -> Failure $ EvaluationError e (show tm)

doEval'
  :: (EnvVal v, Evaluatable tm)
  => Context -> [v] -> tm -> TypeCheckResult (EvalRes tm)
doEval' ctx p tm =
  case evaluate tm (ctxEnv ctx ||><| fromList p) of
    Success v -> return v
    Failure e -> Failure $ EvaluationError e (show tm)

doConv
  :: (ConvCheck v, Quotable v, Show (QuoteRes v))
  => Context -> v -> v -> TypeCheckResult ()
doConv ctx lhs rhs = case execConv (ctxLvl ctx) $ conv (ctxLvl ctx) lhs rhs of
  Success c ->
    if c then
      return ()
    else do
      lhsTm <- doQuote ctx lhs
      rhsTm <- doQuote ctx rhs
      throwError $ UnificationFailure (show lhsTm) (show rhsTm)
  Failure e -> do
    lhsTm <- doQuote ctx lhs
    rhsTm <- doQuote ctx rhs
    throwError $ ConversionError e (show lhsTm) (show rhsTm)

doEvalVTeleSeq :: Context -> Telescope -> TypeCheckResult VTeleSequence
doEvalVTeleSeq ctx tele = case evaluate tele (ctxEnv ctx) >>= intoTeleSequence of
  Success v -> return v
  Failure e -> Failure $ EvaluationError e (show tele)

doIntoTeleSequence :: VTelescope -> TypeCheckResult VTeleSequence
doIntoTeleSequence tele = case intoTeleSequence tele of
  Success v -> return v
  Failure e -> Failure $ EvaluationError e "intoTeleSeq"

doEvalHOAS :: (Environment -> Environment) -> HOAS v -> TypeCheckResult v
doEvalHOAS f hoas = case evalHOAS hoas f of
  Success v -> return v
  Failure e -> Failure $ HoasEvaluationError e

doQuote :: (Quotable v) => Context -> v -> TypeCheckResult (QuoteRes v)
doQuote ctx = doQuote' (ctxLvl ctx)

doQuote' :: (Quotable v) => LevelId -> v -> TypeCheckResult (QuoteRes v)
doQuote' lvl v = case quote lvl v of
  Success t -> return t
  Failure e -> Failure $ QuoteError e

doEvalApp :: LevelId -> Value -> Value -> TypeCheckResult Value
doEvalApp lvl f p = case evaluateApp f p of
  Success v -> return v
  Failure e -> do
    fTm <- doQuote' lvl f
    pTm <- doQuote' lvl p
    Failure $ ApplicationEvaluationError e (show fTm) (show pTm)

doEvalMApp :: LevelId -> MetaValue -> MetaValue -> TypeCheckResult MetaValue
doEvalMApp lvl f p = case evaluateMApp f p of
  Success v -> return v
  Failure e -> do
    fTm <- doQuote' lvl f
    pTm <- doQuote' lvl p
    Failure $ ApplicationEvaluationError e (show fTm) (show pTm)
