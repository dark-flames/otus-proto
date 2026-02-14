module Otus.Normalize.Eval (
  evaluateApp,
  evaluateMApp,
  Evaluatable (..),
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import Otus.Normalize.Value

class (Show t) => Evaluatable t where
  type EvalRes t
  type ClsParam t

  evaluate :: t -> Environment -> EvalResult (EvalRes t)

  evaluateClosure
    :: (EnvVal (ClsParam t))
    => ClsParam t -> Closure t -> EvalResult (EvalRes t)
  evaluateClosure param cls = evaluate (clsTm cls) (clsEnv cls ||> param)

  evaluateClosureFresh
    :: (Domain (ClsParam t))
    => Closure t -> EvalResult (EvalRes t)
  evaluateClosureFresh cls = evaluate (clsTm cls) (env ||> p)
    where
      env = clsEnv cls
      p = intoItem (domVar @(ClsParam t) $ envLevel env)

  evaluateClosureN
    :: (EnvVal (ClsParam t))
    => Seq (ClsParam t) -> Closure t -> EvalResult (EvalRes t)
  evaluateClosureN params cls = evaluate (clsTm cls) (clsEnv cls ||><| params)

  evaluateClosureFreshN
    :: (Domain (ClsParam t))
    => Int -> Closure t -> EvalResult (EvalRes t)
  evaluateClosureFreshN n cls = evaluate (clsTm cls) env'
    where
      env = clsEnv cls
      s = size env
      f = domVar @(ClsParam t) . LevelId
      env' = env ||><| fmap f (fromList [s .. s + n])

-- object
evaluateApp :: Value -> Value -> EvalResult Value
evaluateApp vFn vParam = case vFn of
  VLam cls -> evaluateClosure vParam cls
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
  MNeutral var MSNil -> return $ Neutral (NSplicing var) SNil
  _ -> throwError SplicingNonMeta

instance Evaluatable Telescope where
  type EvalRes Telescope = VTelescope
  type ClsParam Telescope = Value
  evaluate tele env = case unTele tele of
    Empty -> return VTNil
    ty :<| rest -> do
      vTy <- evaluate ty env
      return $ VTCons vTy (Closure env (TeleSeq rest))

instance Evaluatable Record where
  type EvalRes Record = VRecord
  type ClsParam Record = Value

  evaluate record env = mapM (`evaluate` env) (unRecord record)

instance Evaluatable Constraint where
  type EvalRes Constraint = VConstraint
  type ClsParam Constraint = Value
  evaluate (TmEq lift lhs rhs) env = do
    let env' = objLiftEnv lift env
    vLhs <- evaluate lhs env'
    vRhs <- evaluate rhs env'
    return $ VTmEq lift vLhs vRhs

instance Evaluatable Problem where
  type EvalRes Problem = VProblem
  type ClsParam Problem = Value

  evaluate prob env = mapM (`evaluate` env) prob

instance Evaluatable Term where
  type EvalRes Term = Value
  type ClsParam Term = Value
  evaluate tm env = case tm of
    Var idx -> case env @? idx of
      Nothing -> throwError $ UnboundIndex idx
      Just (MetaVal _) -> throwError $ InvalidMetaVar idx
      Just (ObjVal val) -> return val
    TyAnnotation t _ -> evaluate t env
    Pi dom cod -> do
      vDom <- evaluate dom env
      let cls = Closure env cod
      return $ VPi vDom cls
    Lam _ body -> return $ VLam (Closure env body)
    App fn param -> do
      vFn <- evaluate fn env
      vParam <- evaluate param env
      evaluateApp vFn vParam
    Record tele -> VRecord <$> evaluate tele env
    List record -> VList <$> evaluate record env
    First l -> evaluate l env >>= evaluateFirst
    Rest l -> evaluate l env >>= evaluateRest
    Splicing meta -> do
      vMeta <- evaluate meta env
      evaluateSplicing vMeta
    Type l -> return $ VType l

-- meta

evaluateMForce :: MetaValue -> EvalResult MetaValue
evaluateMForce = \case
  MVThunk c -> return c
  MNeutral h spine -> return $ MNeutral h (MSForce spine)
  _ -> throwError ForceOnNonValue

evaluateMApp :: MetaValue -> MetaValue -> EvalResult MetaValue
evaluateMApp vFn vParam = case vFn of
  MVLam cls -> evaluateClosure vParam cls
  MVTrigger e -> return $ MVTrigger e
  MNeutral h spine -> return $ MNeutral h (MSApp spine vParam)
  _ -> throwError AppOnNonLambda

evaluateMBind :: MetaValue -> MetaClosure -> MetaClosure -> EvalResult MetaValue
evaluateMBind prev curCls tyCls = case prev of
  MVTrigger e -> return $ MVTrigger e
  MVReturn val -> evaluateClosure val curCls
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

instance Evaluatable MetaTerm where
  type EvalRes MetaTerm = MetaValue
  type ClsParam MetaTerm = MetaValue
  evaluate tm env = case tm of
    -- Value
    MVar idx -> case env @? idx of
      Nothing -> throwError $ UnboundIndex idx
      Just (ObjVal _) -> throwError $ InvalidObjVar idx
      Just (MetaVal val) -> return val
    MTyAnnotation t _ -> evaluate t env
    MU effs ty -> do
      vTy <- evaluate ty env
      return $ MVU effs vTy
    MThunk ctm -> MVThunk <$> evaluate ctm env
    MVType lvl -> return $ MVVType lvl
    MLift oty -> MVLift <$> evaluate oty env
    MQuote otm -> MVQuote <$> evaluate otm env
    MDyn meta tele -> do
      vMeta <- evaluate meta env
      return $ MVDyn vMeta (Closure env tele)
    MNil lift -> return $ MVNil lift
    MExt prev lift prob s -> do
      vPrev <- evaluate prev env
      vProb <- evaluate prob (objLiftEnv lift env)
      return $ MVExt vPrev lift vProb env s
    -- Computation
    MPi dom eff cod -> do
      vDom <- evaluate dom env
      let cls = Closure env cod
      return $ MVPi vDom eff cls
    MLam _ body -> return $ MVLam (Closure env body)
    MApp f p -> do
      vP <- evaluate p env
      cF <- evaluate f env
      evaluateMApp cF vP
    MF ty -> do
      vTy <- evaluate ty env
      return $ MVF vTy
    MReturn t -> MVReturn <$> evaluate t env
    MTrigger e -> return $ MVTrigger e
    MCType lvl -> return $ MVCType lvl
    MLetIn prev cur bindTy -> do
      cPrev <- evaluate prev env
      evaluateMBind cPrev (Closure env cur) (Closure env bindTy)
    MForce t -> do
      val <- evaluate t env
      evaluateMForce val
    MSolve t -> do
      val <- evaluate t env
      evaluateSolve val
