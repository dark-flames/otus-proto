module Otus.Normalize.Eval (
  absRefl,
  evaluateApp,
  evaluateFirst,
  evaluateRest,
  evaluateJ,
  evaluateMApp,
  evaluateNeutral,
  evaluateTerm,
  intoTeleSequence,
  Evaluatable (..),
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import Otus.Normalize.Unify.State
import Otus.Normalize.Unify.Unify
import Otus.Normalize.Value

class (Show t) => Evaluatable t where
  type EvalRes t

  evaluate :: t -> Environment -> EvalResult (EvalRes t)

  makeCls :: t -> Environment -> HOAS (EvalRes t)
  makeCls t env = HOAS (\f -> evaluate t (f env))

-- object
intoTeleSequence :: VTelescope -> EvalResult VTeleSequence
intoTeleSequence t = VTeleSeq <$> go t
  where
    go VTNil = return Empty
    go (VTCons ty rstHOAS) = do
      rst <- evalHOAS rstHOAS liftObjEnv
      rstSeq <- go rst
      return $ ty :<| rstSeq

evaluateApp :: Value -> Value -> EvalResult Value
evaluateApp vFn vParam = case vFn of
  VLam bodyHOAS -> evalHOAS bodyHOAS (pushEnv vParam)
  Neutral h spine -> return $ Neutral h (SPApp spine vParam)
  _ -> throwError AppOnNonLambda

evaluateFirst :: Value -> EvalResult Value
evaluateFirst = \case
  VList (val :<| _) -> return val
  VList _ -> throwError ProjOnEmptyRecord
  Neutral h spine -> return $ Neutral h (SPFirst spine)
  _ -> throwError ProjOnNonRecord

evaluateRest :: Value -> EvalResult Value
evaluateRest = \case
  VList (_ :<| rest) -> return $ VList rest
  VList _ -> throwError ProjOnEmptyRecord
  Neutral h spine -> return $ Neutral h (SPRest spine)
  _ -> throwError ProjOnNonRecord

evaluateJ :: Value -> Value -> Value -> EvalResult Value
evaluateJ fam p = \case
  VRefl -> return p
  Neutral h spine -> return $ Neutral h (SPJ fam p spine)
  _ -> throwError JOnNonId

evaluateSplicing :: MetaValue -> EvalResult Value
evaluateSplicing = \case
  MVQuote vRecord -> return $ VList vRecord
  MNeutral var MSPNil -> return $ Neutral (NSplicing var) SPNil
  _ -> throwError SplicingNonMeta

evaluateNeutral :: Value -> Spine -> EvalResult Value
evaluateNeutral val = \case
  SPNil -> return val
  SPApp spine p -> do
    f <- evaluateNeutral val spine
    evaluateApp f p
  SPFirst spine -> evaluateNeutral val spine >>= evaluateFirst
  SPRest spine -> evaluateNeutral val spine >>= evaluateRest
  SPJ fam p spine -> evaluateNeutral val spine >>= evaluateJ fam p

evaluateTerm :: Term -> Environment -> EvalResult Value
evaluateTerm = evaluate

instance Evaluatable Telescope where
  type EvalRes Telescope = VTelescope

  evaluate tele env = case unTele tele of
    Empty -> return VTNil
    ty :<| rest -> do
      vTy <- evaluate ty env
      return $ VTCons vTy (makeCls (TeleSeq rest) env)

instance Evaluatable Record where
  type EvalRes Record = VRecord

  evaluate record env = mapM (`evaluate` env) (unRecord record)

instance Evaluatable Constraint where
  type EvalRes Constraint = VConstraint

  evaluate constr env = case constr of
    (TmEq tele lhs rhs eqTy) -> do
      vTeleSeq <- evaluate tele env >>= intoTeleSequence
      let env' = liftObjEnvN (size tele) env
      vEqTy <- evaluate eqTy env'
      vLhs <- evaluate lhs env'
      vRhs <- evaluate rhs env'
      return $ VTmEq vTeleSeq vLhs vRhs vEqTy
    MetaDef ty -> VMetaDef <$> evaluate ty env

instance Evaluatable Problem where
  type EvalRes Problem = VProblem

  evaluate p env = case p of
    Empty -> return Empty
    c :<| prob -> do
      vC <- evaluate c env
      vProb <- evaluate prob (liftObjEnv env)
      return $ vC :<| vProb

instance Evaluatable Term where
  type EvalRes Term = Value

  evaluate tm env = case tm of
    Var idx -> case env @? idx of
      Nothing -> throwError $ UnboundIndex idx
      Just (MetaVal _) -> throwError $ InvalidMetaVar idx
      Just (ObjVal val) -> return val
    TyAnnotation t _ -> evaluate t env
    Pi dom cod -> do
      vDom <- evaluate dom env
      return $ VPi vDom (makeCls cod env)
    Lam _ body -> return $ VLam (makeCls body env)
    App fn param -> do
      vFn <- evaluate fn env
      vParam <- evaluate param env
      evaluateApp vFn vParam
    Record tele -> VRecord <$> evaluate tele env
    List record -> VList <$> evaluate record env
    First l -> evaluate l env >>= evaluateFirst
    Rest l -> evaluate l env >>= evaluateRest
    Id ty l r -> do
      vTy <- evaluate ty env
      vL <- evaluate l env
      vR <- evaluate r env
      return $ VId vTy vL vR
    Refl -> return VRefl
    J fam p e -> do
      vFam <- evaluate fam (liftObjEnvN 2 env)
      vP <- evaluate p env
      vE <- evaluate e env
      evaluateJ vFam vP vE
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
  MVLam bodyHOAS -> evalHOAS bodyHOAS (pushEnv vParam)
  MVTrigger e -> return $ MVTrigger e
  MNeutral h spine -> return $ MNeutral h (MSPApp spine vParam)
  _ -> throwError AppOnNonLambda

evaluateMBind :: MetaValue -> MetaHOAS -> MetaHOAS -> EvalResult MetaValue
evaluateMBind prev curHOAS tyHOAS = case prev of
  MVTrigger e -> return $ MVTrigger e
  MVReturn val -> evalHOAS curHOAS (pushEnv val)
  MNeutral h spine -> return $ MNeutral h (MSBind spine curHOAS tyHOAS)
  _ -> throwError BindOnNonComputation

evaluateMExt :: MetaValue -> Int -> ProblemHOAS -> HOAS VRecord -> EvalResult MetaValue
evaluateMExt prev lift probHOAS recordHOAS = case prev of
  MVGuard prevProb prevRecordHOAS -> do
    prevRecord <- evalHOAS prevRecordHOAS (liftObjEnvN (size prevProb))
    prob <- evalHOAS probHOAS (pushEnvN prevRecord)
    let record f =
          ( do
              -- get the value of prevMeta and prevProb, and then evaluate prevRecord
              pr <- evalHOAS prevRecordHOAS (fst . splitEnv (size prob) . f)
              -- push prevRecord and prob
              let pushRecordWithProb e = e ||><| pr ||><| snd (splitEnv (size prob) (f e))
              evalHOAS recordHOAS pushRecordWithProb
          )
    return $ MVGuard (prevProb >< prob) (HOAS record)
  MNeutral h spine -> return $ MNeutral h (MSExt spine lift probHOAS recordHOAS)
  _ -> throwError AbsOnNonDyn

absRefl :: Int -> Value
absRefl = \case
  x | x > 0 -> VLam (makeCls (lamN (x - 1) Refl) emptyEnv)
  _ -> VRefl

evaluateSolve :: LevelId -> MetaValue -> EvalResult MetaValue
evaluateSolve lvl = \case
  MVGuard prob recordHOAS -> do
    solveRes <- execUnifyMonad (solveProblem lvl prob) (emptyUnifyEnv lvl)
    case solveRes of
      Consistent solutionRecord -> do
        record <- evalHOAS recordHOAS (pushEnvN solutionRecord)
        return $ MVQuote record
      Conflict -> return $ MVTrigger Unification
  MNeutral h spine -> return $ MNeutral h (MSSolve spine)
  _ -> throwError SolveOnNonDyn

instance Evaluatable MetaTerm where
  type EvalRes MetaTerm = MetaValue

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
    MDyn tele -> do
      vtele <- evaluate tele env
      return $ MVDyn vtele
    MGuard prob record -> do
      vProblem <- evaluate prob env
      return $ MVGuard vProblem (makeCls record env)
    MExt prev lift prob record -> do
      vPrev <- evaluate prev env
      evaluateMExt vPrev lift (makeCls prob env) (makeCls record env)
    -- Computation
    MPi dom eff cod -> do
      vDom <- evaluate dom env
      return $ MVPi vDom eff (makeCls cod env)
    MLam _ body -> return $ MVLam (makeCls body env)
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
      evaluateMBind cPrev (makeCls cur env) (makeCls bindTy env)
    MForce t -> do
      val <- evaluate t env
      evaluateMForce val
    MSolve t -> do
      val <- evaluate t env
      evaluateSolve (envLevel env) val
