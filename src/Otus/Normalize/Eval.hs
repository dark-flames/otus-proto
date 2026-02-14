module Otus.Normalize.Eval (
  evaluateMApp,
  evaluateMetaClosure,
  evaluateMetaClosureFresh,
  evaluateMetaValue,
  evaluateMetaComputation,
  evaluate,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import Otus.Normalize.Value

-- meta
evaluateMetaClosure :: MetaValue -> MetaClosure -> EvalResult MetaValue
evaluateMetaClosure p cls = evaluateMetaComputation (clsTm cls) (clsEnv cls ||> p)

evaluateMetaClosureFresh :: MetaClosure -> EvalResult MetaValue
evaluateMetaClosureFresh cls = evaluateMetaComputation (clsTm cls) (freshMetaVar' $ clsEnv cls)

evaluateMForce :: MetaValue -> EvalResult MetaValue
evaluateMForce = \case
  MVThunk c -> return c
  MNeutral h spine -> return $ MNeutral h (MSForce spine)
  _ -> throwError ForceOnNonValue

evaluateMApp :: MetaValue -> MetaValue -> EvalResult MetaValue
evaluateMApp vFn vParam = case vFn of
  MVLam cls -> evaluateMetaClosure vParam cls
  MVTrigger e -> return $ MVTrigger e
  MNeutral h spine -> return $ MNeutral h (MSApp spine vParam)
  _ -> throwError AppOnNonLambda

evaluateMBind :: MetaValue -> MetaClosure -> MetaClosure -> EvalResult MetaValue
evaluateMBind prev curCls tyCls = case prev of
  MVTrigger e -> return $ MVTrigger e
  MVReturn val -> evaluateMetaClosure val curCls
  MNeutral h spine -> return $ MNeutral h (MSBind spine curCls tyCls)
  _ -> throwError BindOnNonComputation

evaluateSolve :: MetaValue -> EvalResult MetaValue
evaluateSolve d = go d mempty mempty
  where
    go v p segs = case v of
      MVNil _lift -> undefined -- todo: solve lift p >>= fold segs
      MVExt prev _ p' env s -> go prev (p' >< p) ((env, s) <| segs)
      MNeutral h spine -> return $ MNeutral h (MSSolveWith spine p segs)
      _ -> throwError SolveOnNonDyn

evaluateMetaValue :: MetaTerm -> Environment -> EvalResult MetaValue
evaluateMetaValue tm env = case tm of
  MVar idx -> case env @? idx of
    Nothing -> throwError $ UnboundIndex idx
    Just (ObjVal _) -> throwError $ InvalidObjVar idx
    Just (MetaVal val) -> return val
  MTyAnnontation t _ -> evaluateMetaValue t env
  MU effs ty -> do
    vTy <- evaluateMetaComputation ty env
    return $ MVU effs vTy
  MThunk ctm -> MVThunk <$> evaluateMetaComputation ctm env
  MVType lvl -> return $ MVVType lvl
  MLift oty -> MVLift . fst <$> evaluateTelescope oty env
  MQuote otm -> MVQuote <$> evaluateRecord otm env
  MDyn meta tele -> do
    (vMeta, env') <- evaluateTelescope meta env
    (vTele, _) <- evaluateTelescope tele env'
    return $ MVDyn vMeta vTele
  MNil lift -> return $ MVNil lift
  MExt prev lift prob s -> do
    vPrev <- evaluateMetaValue prev env
    vProb <- evaluateProblem lift prob env
    return $ MVExt vPrev lift vProb env s
  _ -> throwError $ Anyhow $ "not value: " ++ show tm

evaluateMetaComputation :: MetaTerm -> Environment -> EvalResult MetaValue
evaluateMetaComputation ctm env = case ctm of
  MTyAnnontation t _ -> evaluateMetaComputation t env
  MPi dom eff cod -> do
    vDom <- evaluateMetaValue dom env
    let cls = Closure env cod
    return $ MVPi vDom eff cls
  MLam _ body -> return $ MVLam (Closure env body)
  MApp f p -> do
    vP <- evaluateMetaValue p env
    cF <- evaluateMetaComputation f env
    evaluateMApp cF vP
  MF ty -> do
    vTy <- evaluateMetaValue ty env
    return $ MVF vTy
  MReturn tm -> MVReturn <$> evaluateMetaValue tm env
  MTrigger e -> return $ MVTrigger e
  MCType lvl -> return $ MVCType lvl
  MLetIn prev cur bindTy -> do
    cPrev <- evaluateMetaComputation prev env
    evaluateMBind cPrev (Closure env cur) (Closure env bindTy)
  MForce tm -> do
    val <- evaluateMetaValue tm env
    evaluateMForce val
  MSolve tm -> do
    val <- evaluateMetaValue tm env
    evaluateSolve val
  _ -> throwError $ Anyhow $ "not computation:" ++ show ctm

-- object

evaluateObjClosure :: Value -> ObjClosure -> EvalResult Value
evaluateObjClosure p cls = evaluate (clsTm cls) (clsEnv cls ||> p)

evaluateTelescope :: Telescope -> Environment -> EvalResult (VTelescope, Environment)
evaluateTelescope tele env = foldlM go (mempty, env) tele
  where
    go :: (VTelescope, Environment) -> Term -> EvalResult (VTelescope, Environment)
    go (prev, e) tm = do
      val <- evaluate tm e
      return (prev |> val, freshVar' e)

evaluateRecord :: Record -> Environment -> EvalResult VRecord
evaluateRecord record env = mapM (`evaluate` env) record

evaluateConstraint :: Constraint -> Environment -> EvalResult VConstraint
evaluateConstraint (TmEq lift lhs rhs) env = do
  let env' = objLiftEnv lift env
  vLhs <- evaluate lhs env'
  vRhs <- evaluate rhs env'
  return $ VTmEq lift vLhs vRhs

evaluateProblem :: Int -> Problem -> Environment -> EvalResult VProblem
evaluateProblem lift prob env = mapM (`evaluateConstraint` objLiftEnv lift env) prob

evaluateApp :: Value -> Value -> EvalResult Value
evaluateApp vFn vParam = case vFn of
  VLam cls -> evaluateObjClosure vParam cls
  Neutral h spine -> return $ Neutral h (SApp spine vParam)
  _ -> throwError AppOnNonLambda

evaluateFirst :: Value -> EvalResult Value
evaluateFirst = \case
  VList (val :<| _) -> return val
  VList _ -> throwError ProjOnEmptyRecord
  Neutral h spine -> return $ Neutral h (SFirst spine)
  _ -> throwError ProjOnNonRecord

evaluateRest :: Value -> EvalResult Value
evaluateRest = \case
  VList (_ :<| rest) -> return $ VList rest
  VList _ -> throwError ProjOnEmptyRecord
  Neutral h spine -> return $ Neutral h (SRest spine)
  _ -> throwError ProjOnNonRecord

evaluateSplicing :: MetaValue -> EvalResult Value
evaluateSplicing = \case
  MVQuote vRecord -> return $ VList vRecord
  MNeutral var spine -> return $ Neutral (NSplicing var spine) SNil
  _ -> throwError SplicingNonMeta

evaluate :: Term -> Environment -> EvalResult Value
evaluate tm env = case tm of
  Var idx -> case env @? idx of
    Nothing -> throwError $ UnboundIndex idx
    Just (MetaVal _) -> throwError $ InvalidMetaVar idx
    Just (ObjVal val) -> return val
  Pi dom cod -> do
    vDom <- evaluate dom env
    let cls = Closure env cod
    return $ VPi vDom cls
  Lam body -> return $ VLam (Closure env body)
  App fn param -> do
    vFn <- evaluate fn env
    vParam <- evaluate param env
    evaluateApp vFn vParam
  Record tele -> VRecord . fst <$> evaluateTelescope tele env
  List record -> VList <$> evaluateRecord record env
  First l -> evaluate l env >>= evaluateFirst
  Rest l -> evaluate l env >>= evaluateRest
  Splicing meta -> do
    vMeta <- evaluateMetaValue meta env
    evaluateSplicing vMeta
  Type l -> return $ VType l
