module Otus.TypeCheck.Error (
  TypeError (..),
  TypeCheckResult,
  doEvalValue,
  doEvalComputation,
  doEvalClosure,
  doEvalClosureFresh,
  doEvalMApp,
  doReadbackMeta,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize

data TypeError
  = ValueEvalError EvalError MetaTerm
  | ComputationEvalError EvalError MetaTerm
  | ReadbackError EvalError MetaValue
  | MAppEvalError EvalError MetaTerm MetaTerm
  | CannotInferIndex
  | CannotInferObjAsMeta
  | CannotCheckAsMetaType MetaTerm MetaTerm
  | CannotCheckAsThunk MetaTerm MetaTerm
  | CannotForce MetaTerm MetaTerm
  | CannotBindOn MetaTerm MetaTerm
  | ComputationUnify MetaTerm MetaTerm MetaTerm
  | ValueUnify MetaTerm MetaTerm MetaTerm
  | CannotInferValue MetaTerm
  | CannotInferComputation MetaTerm
  | ComputationEffErr MetaTerm EffectSet EffectSet
  | ExpectedToBeComputationTy MetaTerm
  | ExpectedToBeValueTy MetaTerm
  | ExpectedToBeMetaPi MetaTerm MetaTerm
  | AnyhowTyErr String
  deriving (Eq, Show)

type TypeCheckResult = Result TypeError

doEvalValue :: MetaTerm -> Environment -> TypeCheckResult MetaValue
doEvalValue tm env =
  -- mapEvalResult $ evaluateMetaValue tm env
  case evaluateMetaValue tm env of
    Success v -> return v
    Failure e -> Failure $ ValueEvalError e tm

doEvalComputation :: MetaTerm -> Environment -> TypeCheckResult MetaValue
doEvalComputation tm env = case evaluateMetaComputation tm env of
  Success v -> return v
  Failure e -> Failure $ ComputationEvalError e tm

doEvalClosure :: MetaValue -> MetaClosure -> TypeCheckResult MetaValue
doEvalClosure val cls = case evaluateMetaClosure val cls of
  Success v -> return v
  Failure e -> Failure $ ComputationEvalError e (clsTm cls)

doEvalClosureFresh :: MetaClosure -> TypeCheckResult MetaValue
doEvalClosureFresh cls = case evaluateMetaClosureFresh cls of
  Success v -> return v
  Failure e -> Failure $ ComputationEvalError e (clsTm cls)

doReadbackMeta :: LevelId -> MetaValue -> TypeCheckResult MetaTerm
doReadbackMeta lvl v = case readbackMeta lvl v of
  Success t -> return t
  Failure e -> Failure $ ReadbackError e v

doEvalMApp :: LevelId -> MetaValue -> MetaValue -> TypeCheckResult MetaValue
doEvalMApp lvl f p = case evaluateMApp f p of
  Success v -> return v
  Failure e -> do
    fTm <- doReadbackMeta lvl f
    pTm <- doReadbackMeta lvl p
    Failure $ MAppEvalError e fTm pTm
