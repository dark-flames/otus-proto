module Otus.TypeCheck.Error (
  TypeError (..),
  TypeCheckResult,
  doEval,
  doEvalValue,
  doEvalComputation,
  doEvalTeleClosure,
  doEvalTeleClosureFresh,
  doEvalClosure,
  doEvalClosureFresh,
  doEvalMetaClosure,
  doEvalMetaClosureFresh,
  doEvalApp,
  doEvalMApp,
  doReadbackTele,
  doReadback,
  doReadbackMeta,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize

data TypeError
  = EvalError EvalError Term
  | TeleEvalError EvalError Telescope
  | ValueEvalError EvalError MetaTerm
  | ComputationEvalError EvalError MetaTerm
  | ReadbackError EvalError Value
  | TelescopeReadbackError EvalError VTelescope
  | MetaReadbackError EvalError MetaValue
  | AppEvalError EvalError Term Term
  | MAppEvalError EvalError MetaTerm MetaTerm
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
  | CannotCheckAsType Term
  | ExpectedToBeComputationTy MetaTerm
  | ExpectedToBeValueTy MetaTerm
  | ExpectedToBeFn Term Term
  | ExpectedToBeMetaFn MetaTerm MetaTerm
  | ExpectedToBeNonEmptyRecord Term Term
  | AnyhowTyErr String
  deriving (Eq, Show)

type TypeCheckResult = Result TypeError

doEval :: Term -> Environment -> TypeCheckResult Value
doEval tm env =
  case evaluate tm env of
    Success v -> return v
    Failure e -> Failure $ EvalError e tm

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

doEvalTeleClosure :: Value -> TeleClosure -> TypeCheckResult VTelescope
doEvalTeleClosure val cls = case evaluateTeleClosure val cls of
  Success v -> return v
  Failure e -> Failure $ TeleEvalError e (clsTm cls)

doEvalTeleClosureFresh :: TeleClosure -> TypeCheckResult VTelescope
doEvalTeleClosureFresh cls = case evaluateTeleClosureFresh cls of
  Success v -> return v
  Failure e -> Failure $ TeleEvalError e (clsTm cls)

doEvalClosure :: Value -> ObjClosure -> TypeCheckResult Value
doEvalClosure val cls = case evaluateClosure val cls of
  Success v -> return v
  Failure e -> Failure $ EvalError e (clsTm cls)

doEvalClosureFresh :: ObjClosure -> TypeCheckResult Value
doEvalClosureFresh cls = case evaluateClosureFresh cls of
  Success v -> return v
  Failure e -> Failure $ EvalError e (clsTm cls)

doEvalMetaClosure :: MetaValue -> MetaClosure -> TypeCheckResult MetaValue
doEvalMetaClosure val cls = case evaluateMetaClosure val cls of
  Success v -> return v
  Failure e -> Failure $ ComputationEvalError e (clsTm cls)

doEvalMetaClosureFresh :: MetaClosure -> TypeCheckResult MetaValue
doEvalMetaClosureFresh cls = case evaluateMetaClosureFresh cls of
  Success v -> return v
  Failure e -> Failure $ ComputationEvalError e (clsTm cls)

doReadbackTele :: LevelId -> VTelescope -> TypeCheckResult Telescope
doReadbackTele lvl tele = case readbackTelescope lvl tele of
  Success t -> return t
  Failure e -> Failure $ TelescopeReadbackError e tele

doReadback :: LevelId -> Value -> TypeCheckResult Term
doReadback lvl v = case readback lvl v of
  Success t -> return t
  Failure e -> Failure $ ReadbackError e v

doReadbackMeta :: LevelId -> MetaValue -> TypeCheckResult MetaTerm
doReadbackMeta lvl v = case readbackMeta lvl v of
  Success t -> return t
  Failure e -> Failure $ MetaReadbackError e v

doEvalApp :: LevelId -> Value -> Value -> TypeCheckResult Value
doEvalApp lvl f p = case evaluateApp f p of
  Success v -> return v
  Failure e -> do
    fTm <- doReadback lvl f
    pTm <- doReadback lvl p
    Failure $ AppEvalError e fTm pTm

doEvalMApp :: LevelId -> MetaValue -> MetaValue -> TypeCheckResult MetaValue
doEvalMApp lvl f p = case evaluateMApp f p of
  Success v -> return v
  Failure e -> do
    fTm <- doReadbackMeta lvl f
    pTm <- doReadbackMeta lvl p
    Failure $ MAppEvalError e fTm pTm
